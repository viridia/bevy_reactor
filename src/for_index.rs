use bevy::ecs::entity::Entity;
use bevy::ecs::world::World;
use bevy::hierarchy::Parent;

use crate::{DespawnScopes, DisplayNodeChanged, Rcx, TrackingScope, View, ViewHandle};

use crate::node_span::NodeSpan;

pub struct IndexedListItem<Item> {
    id: Entity,
    view: ViewHandle,
    value: Item,
}

#[doc(hidden)]
pub struct ForIndex<
    Item: PartialEq + Clone + 'static,
    ItemIter: Iterator<Item = Item>,
    ItemFn: Fn(&Rcx) -> ItemIter,
    V: Into<ViewHandle>,
    F: Fn(&Item, usize) -> V,
> {
    item_fn: ItemFn,
    each_fn: F,
    items: Vec<IndexedListItem<Item>>,
    marker: std::marker::PhantomData<Item>,
    fallback: Option<ViewHandle>,
    fallback_ent: Option<Entity>,
}

impl<
        Item: PartialEq + Clone + 'static,
        ItemIter: Iterator<Item = Item>,
        ItemFn: Fn(&Rcx) -> ItemIter,
        V: Into<ViewHandle>,
        F: Fn(&Item, usize) -> V,
    > ForIndex<Item, ItemIter, ItemFn, V, F>
{
    pub fn new(item_fn: ItemFn, each_fn: F) -> Self {
        Self {
            item_fn,
            each_fn,
            items: Vec::new(),
            marker: std::marker::PhantomData,
            fallback: None,
            fallback_ent: None,
        }
    }

    /// Allow specifying a fallback view to render if there are no items.
    pub fn with_fallback<FB: Into<ViewHandle>>(mut self, fallback: FB) -> Self {
        self.fallback = Some(fallback.into());
        self
    }
}

impl<
        Item: PartialEq + Clone + 'static,
        ItemIter: Iterator<Item = Item>,
        ItemFn: Fn(&Rcx) -> ItemIter,
        V: Into<ViewHandle>,
        F: Fn(&Item, usize) -> V,
    > View for ForIndex<Item, ItemIter, ItemFn, V, F>
{
    fn nodes(&self) -> NodeSpan {
        let child_spans: Vec<NodeSpan> = self.items.iter().map(|item| item.view.nodes()).collect();
        NodeSpan::Fragment(child_spans.into_boxed_slice())
    }

    fn build(&mut self, view_entity: bevy::prelude::Entity, world: &mut World) {
        let mut tracking = TrackingScope::new(world.read_change_tick());
        self.react(view_entity, world, &mut tracking);
        world.entity_mut(view_entity).insert(tracking);
        assert!(
            world.entity_mut(view_entity).get::<Parent>().is_some(),
            "ForIndex should have a parent view"
        );
    }

    fn react(
        &mut self,
        view_entity: bevy::prelude::Entity,
        world: &mut World,
        tracking: &mut crate::TrackingScope,
    ) {
        let iter = (self.item_fn)(&Rcx::new(world, tracking));
        let mut prev_len = self.items.len();
        let mut changed = false;

        let mut index = 0usize;
        for item in iter {
            if index < prev_len {
                // Overwrite existing items.
                let entry = &mut self.items[index];
                if item != entry.value {
                    entry.view.raze(entry.id, world);
                    entry.value = item.clone();
                    entry.view = (self.each_fn)(&entry.value, index).into();
                    entry.id = ViewHandle::spawn(&entry.view, view_entity, world);
                    changed = true;
                }
            } else {
                // Append new items.
                let view = (self.each_fn)(&item, index).into();
                let id = ViewHandle::spawn(&view, view_entity, world);
                self.items.push(IndexedListItem {
                    id,
                    view,
                    value: item.clone(),
                });
                changed = true;
            }
            index += 1;
        }

        // Raze surplus items.
        while index < prev_len {
            prev_len -= 1;
            let entry = &mut self.items[prev_len];
            entry.view.raze(entry.id, world);
            self.items.pop();
        }

        // Handle fallback
        if let Some(ref mut fallback) = self.fallback {
            match self.fallback_ent {
                // If there are > 0 items, destroy fallback if present.
                Some(fb_ent) if index > 0 => {
                    fallback.raze(fb_ent, world);
                    self.fallback_ent = None;
                    changed = true;
                }

                // If there are no items, render fallback unless already rendered.
                None if index == 0 => {
                    self.fallback_ent = Some(ViewHandle::spawn(fallback, view_entity, world));
                    changed = true;
                }

                // Otherwise, no change.
                _ => {}
            }
        }

        if changed {
            world.entity_mut(view_entity).insert(DisplayNodeChanged);
        }
    }

    fn raze(&mut self, view_entity: bevy::prelude::Entity, world: &mut World) {
        for entry in self.items.drain(..) {
            entry.view.raze(entry.id, world);
        }
        world.despawn_owned_recursive(view_entity);
    }
}

impl<
        Item: Send + Sync + Clone + PartialEq + 'static,
        ItemIter: 'static + Iterator<Item = Item>,
        ItemFn: Send + Sync + 'static + Fn(&Rcx) -> ItemIter,
        V: 'static + Into<ViewHandle>,
        F: Send + Sync + 'static + Fn(&Item, usize) -> V,
    > From<ForIndex<Item, ItemIter, ItemFn, V, F>> for ViewHandle
{
    fn from(value: ForIndex<Item, ItemIter, ItemFn, V, F>) -> Self {
        ViewHandle::new(value)
    }
}
