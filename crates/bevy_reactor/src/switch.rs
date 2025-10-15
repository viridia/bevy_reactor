#![allow(clippy::type_complexity)]
use std::{
    marker::PhantomData,
    sync::{Arc, Mutex},
};

use bevy::{
    prelude::*,
    scene2::{Scene, SceneList, SpawnRelatedScenes},
    ui::experimental::GhostNode,
};

use crate::{
    Cx, TrackingScope,
    cx::Lens,
    owner::OwnedBy,
    reaction::{InitialReactionCommand, Reaction, ReactionCell},
};

/// Trait that represents a function that can produce a [`SceneList`].
pub trait SceneListFn: Send + Sync {
    fn spawn(&self, parent: EntityCommands);
}

impl<S: SceneList, F: Fn() -> S + Send + Sync + 'static> SceneListFn for F {
    fn spawn(&self, parent: EntityCommands) {
        parent.spawn_related_scenes::<Children>((self)());
    }
}

pub struct CaseBuilder<Value: Send + Sync> {
    cases: Vec<(Value, Box<dyn SceneListFn + Send + Sync>)>,
    fallback: Option<Box<dyn SceneListFn + Send + Sync>>,
}

impl<Value: Send + Sync> CaseBuilder<Value> {
    pub fn case<CF: Send + Sync + 'static + SceneListFn>(
        &mut self,
        value: Value,
        case_fn: CF,
    ) -> &mut Self {
        self.cases.push((value, Box::new(case_fn)));
        self
    }

    pub fn fallback<FF: Send + Sync + 'static + SceneListFn>(
        &mut self,
        fallback_fn: FF,
    ) -> &mut Self {
        self.fallback = Some(Box::new(fallback_fn));
        self
    }
}

/// Conditional control-flow node that implements a C-like "switch" statement.
struct SwitchReaction<Value: Send + Sync + 'static, SelectorFn: Lens<Value>> {
    switch_index: usize,
    selector_fn: SelectorFn,
    cases: Arc<Mutex<CaseBuilder<Value>>>,
}

impl<Value: PartialEq + Send + Sync + 'static, SelectorFn: Lens<Value>> Reaction
    for SwitchReaction<Value, SelectorFn>
{
    fn react(&mut self, owner: Entity, world: &mut World, tracking: &mut TrackingScope) {
        // Run the condition and see if the result changed.
        let re = Cx::new(world, owner, tracking);
        let value: Value = self.selector_fn.call(&re);
        let mut cases = self.cases.lock().unwrap();
        let index = cases
            .cases
            .iter()
            .enumerate()
            .find_map(|(i, f)| if f.0 == value { Some(i) } else { None })
            .unwrap_or(usize::MAX);

        // popup.insert(Node::default());
        if self.switch_index != index {
            self.switch_index = index;
            let mut commands = world.commands();
            let mut entt = commands.entity(owner);
            entt.despawn_related::<Children>();
            // entt.despawn_related::<Owned>();
            if index < cases.cases.len() {
                cases.cases[index].1.spawn(entt);
            } else if let Some(fallback) = cases.fallback.as_mut() {
                fallback.spawn(entt);
            };
        }
    }
}

pub struct Switch<Value: PartialEq + Send + Sync + 'static, SelectorFn: Lens<Value>> {
    selector_fn: SelectorFn,
    cases: Arc<Mutex<CaseBuilder<Value>>>,
    marker: std::marker::PhantomData<Value>,
}

impl<Value: PartialEq + Send + Sync + 'static, SelectorFn: Lens<Value>> Switch<Value, SelectorFn> {
    pub fn new<CF: Fn(&mut CaseBuilder<Value>)>(value_fn: SelectorFn, cases_fn: CF) -> Self {
        let mut case_builder = CaseBuilder {
            cases: Vec::new(),
            fallback: None,
        };
        cases_fn(&mut case_builder);

        Self {
            selector_fn: value_fn,
            cases: Arc::new(Mutex::new(case_builder)),
            marker: std::marker::PhantomData,
        }
    }
}

impl<
    Value: PartialEq + Send + Sync + 'static,
    SelectorFn: Lens<Value> + Clone + Send + Sync + 'static,
> Template for Switch<Value, SelectorFn>
{
    type Output = ();

    fn build(&mut self, parent: &mut EntityWorldMut) -> Result<Self::Output> {
        let parent_id = parent.id();
        parent.world_scope(|world| {
            let ticks = world.change_tick();
            let scope = TrackingScope::new(ticks);
            let reaction = world
                .spawn((
                    ReactionCell::new(SwitchReaction {
                        cases: self.cases.clone(),
                        selector_fn: self.selector_fn.clone(),
                        switch_index: usize::MAX - 1, // Means no case selected yet.
                    }),
                    scope,
                    GhostNode,
                ))
                .id();

            // Add the reaction to the target entity
            world
                .entity_mut(parent_id)
                .add_one_related::<OwnedBy>(reaction);

            // Set up to run the reaction after creation.
            world.commands().queue(InitialReactionCommand(reaction));
        });

        Ok(())
    }
}

impl<
    Value: PartialEq + Send + Sync + 'static,
    SelectorFn: Lens<Value> + Clone + Send + Sync + 'static,
> Scene for Switch<Value, SelectorFn>
{
    fn patch(
        &self,
        _assets: &AssetServer,
        _patches: &Assets<bevy::scene2::ScenePatch>,
        scene: &mut bevy::scene2::ResolvedScene,
    ) {
        scene.push_template(Switch {
            selector_fn: self.selector_fn.clone(),
            cases: self.cases.clone(),
            marker: PhantomData,
        });
    }
}

pub fn switch<
    Value: PartialEq + Send + Sync + 'static,
    SelectorFn: Lens<Value> + Clone + Send + Sync + 'static,
    CF: Fn(&mut CaseBuilder<Value>),
>(
    selector_fn: SelectorFn,
    case_fn: CF,
) -> impl Scene {
    Switch::<Value, SelectorFn>::new(selector_fn, case_fn)
}
