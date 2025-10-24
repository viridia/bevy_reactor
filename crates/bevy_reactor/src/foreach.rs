#![allow(clippy::type_complexity)]
use std::{marker::PhantomData, ops::Range, sync::Arc};

use bevy::{
    ecs::template::TemplateContext,
    prelude::*,
    scene2::{Scene, SceneList, SpawnRelatedScenes as _},
    ui::experimental::GhostNode,
};

use crate::{
    Cx, SceneListFn, TrackingScope,
    cx::Lens,
    lcs::lcs,
    reaction::{InitialReactionCommand, Reaction, ReactionCell},
};

/// Trait that represents a function that can produce a [`SceneList`] for the body of a for
/// loop.
pub trait LoopBodyFn<Item>: Send + Sync {
    fn spawn(&self, parent: EntityWorldMut, item: &Item, index: usize);
}

impl<S: SceneList, Item, F: Fn(&Item, usize) -> S + Send + Sync + 'static> LoopBodyFn<Item> for F {
    fn spawn(&self, parent: EntityWorldMut, item: &Item, index: usize) {
        parent.spawn_related_scenes::<Children>((self)(item, index));
    }
}

#[derive(Clone)]
struct ChildRow<Item: Clone> {
    child: Entity,
    item: Item,
}

/// Conditional control-flow node that implements a C-like "switch" statement.
struct ForEachReaction<
    Item: Clone + PartialEq + Send + Sync + 'static,
    ItemFn: Lens<Vec<Item>> + Clone + Send + Sync + 'static,
    CmpFn: Fn(&Item, &Item) -> bool + Clone + Send + Sync + 'static,
    EachFn: LoopBodyFn<Item>,
> {
    item_fn: ItemFn,
    each_fn: EachFn,
    fallback_fn: Option<Arc<dyn SceneListFn>>,
    cmp_fn: CmpFn,
    marker: PhantomData<Item>,
    state: Vec<ChildRow<Item>>,
    first: bool,
}

impl<
    Item: Clone + PartialEq + Send + Sync + 'static,
    ItemFn: Lens<Vec<Item>> + Clone + Send + Sync + 'static,
    CmpFn: Fn(&Item, &Item) -> bool + Clone + Send + Sync + 'static,
    EachFn: LoopBodyFn<Item>,
> ForEachReaction<Item, ItemFn, CmpFn, EachFn>
{
    /// Uses the sequence of key values to match the previous array items with the updated
    /// array items. Matching items are patched, other items are inserted or deleted.
    ///
    /// # Arguments
    ///
    /// * `world` - [`World`] used to spawn row entities.
    /// * `prev_state` - Array of view state elements from previous update.
    /// * `prev_range` - The range of elements we are comparing in `prev_state`.
    /// * `next_state` - Array of view state elements to be built.
    /// * `next_range` - The range of elements we are comparing in `next_state`.
    /// * `out` - List of child rows being built.
    #[allow(clippy::too_many_arguments, clippy::needless_range_loop)]
    fn build_recursive(
        &self,
        world: &mut World,
        prev_state: &[ChildRow<Item>],
        prev_range: Range<usize>,
        next_items: &[Item],
        next_range: Range<usize>,
        out: &mut Vec<ChildRow<Item>>,
    ) {
        // Look for longest common subsequence.
        // prev_start and next_start are *relative to the slice*.
        let (prev_start, next_start, lcs_length) = lcs(
            &prev_state[prev_range.clone()],
            &next_items[next_range.clone()],
            |a, b| (self.cmp_fn)(&a.item, b),
        );

        // If there was nothing in common
        if lcs_length == 0 {
            // Raze old elements
            for i in prev_range {
                let prev = &prev_state[i];
                world.entity_mut(prev.child).despawn();
            }
            // Build new elements
            for i in next_range {
                let child = world.spawn(GhostNode);
                let child_id = child.id();
                self.each_fn.spawn(child, &next_items[i], i);
                out.push(ChildRow {
                    child: child_id,
                    item: next_items[i].clone(),
                });
            }
            return;
        }

        // Adjust prev_start and next_start to be relative to the entire state array.
        let prev_start = prev_start + prev_range.start;
        let next_start = next_start + next_range.start;

        // Stuff that precedes the LCS.
        if prev_start > prev_range.start {
            if next_start > next_range.start {
                // Both prev and next have entries before lcs, so recurse
                self.build_recursive(
                    world,
                    // owner,
                    prev_state,
                    prev_range.start..prev_start,
                    next_items,
                    next_range.start..next_start,
                    out,
                )
            } else {
                // Deletions
                for i in prev_range.start..prev_start {
                    let prev = &prev_state[i];
                    world.entity_mut(prev.child).despawn();
                }
            }
        } else if next_start > next_range.start {
            // Insertions
            for i in next_range.start..next_start {
                let child = world.spawn(GhostNode);
                let child_id = child.id();
                self.each_fn.spawn(child, &next_items[i], i);
                out.push(ChildRow {
                    child: child_id,
                    item: next_items[i].clone(),
                });
            }
        }

        // For items that match, copy over the view and value.
        for i in 0..lcs_length {
            let prev = &prev_state[prev_start + i];
            out.push(prev.clone());
        }

        // Stuff that follows the LCS.
        let prev_end = prev_start + lcs_length;
        let next_end = next_start + lcs_length;
        if prev_end < prev_range.end {
            if next_end < next_range.end {
                // Both prev and next have entries after lcs, so recurse
                self.build_recursive(
                    world,
                    // owner,
                    prev_state,
                    prev_end..prev_range.end,
                    next_items,
                    next_end..next_range.end,
                    out,
                );
            } else {
                // Deletions
                for i in prev_end..prev_range.end {
                    let prev = &prev_state[i];
                    world.entity_mut(prev.child).despawn();
                }
            }
        } else if next_end < next_range.end {
            // Insertions
            for i in next_end..next_range.end {
                let child = world.spawn(GhostNode);
                let child_id = child.id();
                self.each_fn.spawn(child, &next_items[i], i);
                out.push(ChildRow {
                    child: child_id,
                    item: next_items[i].clone(),
                });
            }
        }
    }
}

impl<
    Item: Clone + PartialEq + Send + Sync + 'static,
    ItemFn: Lens<Vec<Item>> + Clone + Send + Sync + 'static,
    CmpFn: Fn(&Item, &Item) -> bool + Clone + Send + Sync + 'static,
    EachFn: LoopBodyFn<Item> + Clone + 'static,
> ForEach<Item, ItemFn, CmpFn, EachFn>
{
    pub fn new<Fallback: SceneListFn + 'static>(
        item_fn: ItemFn,
        each_fn: EachFn,
        cmp_fn: CmpFn,
        fallback: Fallback,
    ) -> Self {
        Self {
            item_fn: item_fn.clone(),
            each_fn: each_fn.clone(),
            cmp_fn: cmp_fn.clone(),
            fallback_fn: Some(Arc::new(fallback)),
            marker: std::marker::PhantomData,
        }
    }
}

impl<
    Item: Clone + PartialEq + Send + Sync + 'static,
    ItemFn: Lens<Vec<Item>> + Clone + Send + Sync + 'static,
    CmpFn: Fn(&Item, &Item) -> bool + Clone + Send + Sync + 'static,
    EachFn: LoopBodyFn<Item>,
> Reaction for ForEachReaction<Item, ItemFn, CmpFn, EachFn>
{
    fn react(&mut self, parent: Entity, world: &mut World, tracking: &mut TrackingScope) {
        // Run the condition and see if the result changed.
        let cx = Cx::new(world, parent, tracking);
        let items: Vec<Item> = self.item_fn.call(&cx);
        let next_len = items.len();
        let prev_len = self.state.len();
        let mut next_state: Vec<ChildRow<Item>> = Vec::with_capacity(items.len());

        self.build_recursive(
            world,
            &self.state,
            0..prev_len,
            &items,
            0..next_len,
            &mut next_state,
        );
        let children: Vec<Entity> = next_state.iter().map(|i| i.child).collect();
        self.state = std::mem::take(&mut next_state);

        let mut commands = world.commands();
        let mut entt = commands.entity(parent);
        if next_len == 0 {
            // Display fallback, if any
            if prev_len > 0 || self.first {
                self.first = false;
                // Transitioning from non-empty to empty, generate fallback.
                entt.despawn_related::<Children>();
                if let Some(fallback) = self.fallback_fn.as_ref() {
                    fallback.spawn(entt);
                }
            }
        } else {
            if prev_len == 0 {
                // Transitioning from non-empty to empty, delete fallback.
                entt.despawn_related::<Children>();
            } else {
                // Remove old children but do not despawn, since some row entities will be reused.
                // Children no longer present will already have been despawned.
                entt.remove::<Children>();
            }
            // Add new children
            entt.add_related::<ChildOf>(&children);
        }
    }
}

pub struct ForEach<
    Item: Clone + PartialEq + Send + Sync + 'static,
    ItemFn: Lens<Vec<Item>> + Clone + Send + Sync + 'static,
    CmpFn: Fn(&Item, &Item) -> bool + Clone + Send + Sync + 'static,
    EachFn: LoopBodyFn<Item>,
> {
    item_fn: ItemFn,
    each_fn: EachFn,
    fallback_fn: Option<Arc<dyn SceneListFn>>,
    cmp_fn: CmpFn,
    marker: PhantomData<Item>,
}

impl<
    Item: Clone + PartialEq + Send + Sync + 'static,
    ItemFn: Lens<Vec<Item>> + Clone + Send + Sync + 'static,
    CmpFn: Fn(&Item, &Item) -> bool + Clone + Send + Sync + 'static,
    EachFn: LoopBodyFn<Item> + Clone + 'static,
> Template for ForEach<Item, ItemFn, CmpFn, EachFn>
{
    type Output = ();

    fn build(&mut self, context: &mut TemplateContext) -> Result<Self::Output> {
        let parent_id = context.entity.id();
        context.entity.world_scope(|world| {
            let ticks = world.change_tick();
            let scope = TrackingScope::new(ticks);
            let reaction = world
                .entity_mut(parent_id)
                .insert((
                    ReactionCell::new(ForEachReaction {
                        item_fn: self.item_fn.clone(),
                        each_fn: self.each_fn.clone(),
                        cmp_fn: self.cmp_fn.clone(),
                        fallback_fn: self.fallback_fn.clone(),
                        state: Vec::new(),
                        first: true,
                        marker: PhantomData,
                    }),
                    scope,
                    GhostNode,
                ))
                .id();

            // Set up to run the reaction after creation.
            world.commands().queue(InitialReactionCommand(reaction));
        });

        Ok(())
    }
}

impl<
    Item: Clone + PartialEq + Send + Sync + 'static,
    ItemFn: Lens<Vec<Item>> + Clone + Send + Sync + 'static,
    CmpFn: Fn(&Item, &Item) -> bool + Clone + Send + Sync + 'static,
    EachFn: LoopBodyFn<Item> + Clone + 'static,
> Scene for ForEach<Item, ItemFn, CmpFn, EachFn>
{
    fn patch(
        &self,
        _context: &mut bevy::scene2::PatchContext,
        scene: &mut bevy::scene2::ResolvedScene,
    ) {
        scene.push_template(ForEach {
            item_fn: self.item_fn.clone(),
            each_fn: self.each_fn.clone(),
            cmp_fn: self.cmp_fn.clone(),
            fallback_fn: self.fallback_fn.clone(),
            marker: PhantomData,
        });
    }
}

pub fn for_each<
    Item: Clone + PartialEq + Send + Sync + 'static,
    ItemFn: Lens<Vec<Item>> + Clone + Send + Sync + 'static,
    S: SceneList,
    EachFn: Fn(&Item, usize) -> S + Send + Sync + Clone + 'static,
    Fallback: SceneListFn + 'static,
>(
    item_fn: ItemFn,
    each_fn: EachFn,
    fallback: Fallback,
) -> impl Scene {
    ForEach::new(item_fn, each_fn, PartialEq::eq, fallback)
}

pub fn for_each_cmp<
    Item: Clone + PartialEq + Send + Sync + 'static,
    ItemFn: Lens<Vec<Item>> + Clone + Send + Sync + 'static,
    CmpFn: Fn(&Item, &Item) -> bool + Clone + Send + Sync + 'static,
    S: SceneList,
    EachFn: Fn(&Item, usize) -> S + Send + Sync + Clone + 'static,
    Fallback: SceneListFn + 'static,
>(
    item_fn: ItemFn,
    each_fn: EachFn,
    cmp_fn: CmpFn,
    fallback: Fallback,
) -> impl Scene {
    ForEach::new(item_fn, each_fn, cmp_fn, fallback)
}
