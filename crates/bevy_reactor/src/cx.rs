use std::cell::RefCell;

use bevy::{
    ecs::{
        hierarchy::ChildOf,
        world::{DeferredWorld, EntityRef},
    },
    prelude::{Component, Entity, Resource, World},
};

use crate::{
    Derived, Mutable, ReadDerived, ReadMutable, Signal, TrackingScope, derived::ReadDerivedInternal,
};

/// Immutable reactive context, used for reactive closures such as derived signals.
pub struct Cx<'p, 'w> {
    /// Bevy World
    pub(crate) world: &'w World,

    /// The entity that owns the tracking scope (or will own it).
    pub(crate) owner: Entity,

    /// Set of reactive resources referenced by the presenter.
    pub(crate) tracking: RefCell<&'p mut TrackingScope>,
}

impl<'p, 'w> Cx<'p, 'w> {
    /// Create a new read-only reactive context.
    pub fn new(world: &'w World, owner: Entity, tracking: &'p mut TrackingScope) -> Self {
        Self {
            world,
            owner,
            tracking: RefCell::new(tracking),
        }
    }

    /// Return an [`EntityRefTracked`] for the owner or target of the current reaction.
    pub fn owner<'a>(&'a self) -> EntityRefTracked<'a, 'p, 'w> {
        EntityRefTracked {
            entity: self.world.entity(self.owner),
            cx: self,
        }
    }

    /// Return an [`EntityRefTracked`] for the given entity.
    pub fn entity<'a>(&'a self, entity: Entity) -> EntityRefTracked<'a, 'p, 'w> {
        EntityRefTracked {
            entity: self.world.entity(entity),
            cx: self,
        }
    }

    /// Access to immutable world from reactive context.
    pub fn world(&self) -> &World {
        self.world
    }

    /// Return a reference to the resource of the given type. Calling this function
    /// adds the resource as a dependency of the current tracking scope.
    pub fn resource<T: Resource>(&self) -> &T {
        self.tracking.borrow_mut().track_resource::<T>(self.world);
        self.world.resource::<T>()
    }

    /// Return a reference to the Component `C` on the given entity. Calling this function
    /// adds the component as a dependency of the current tracking scope.
    pub fn component<C: Component>(&self, entity: Entity) -> Option<&C> {
        let comp = self.world.entity(entity).get::<C>();
        self.tracking
            .borrow_mut()
            .track_component::<C>(entity, self.world, comp.is_some());
        comp
    }

    /// Return a reference to the Component `C` on the owner entity of the current
    /// context, or one of it's ancestors. This searches up the entity tree until it finds
    /// a component of the given type.
    pub fn get_inherited_component<C: Component>(&self) -> Option<&C> {
        let mut entity = self.owner;
        loop {
            let ec = self.component(entity);
            if ec.is_some() {
                return ec;
            }
            match self.world.entity(entity).get::<ChildOf>() {
                Some(parent) => entity = parent.parent(),
                _ => return None,
            }
        }
    }

    /// Add a cleanup function which is run once before the next reaction, or when the owner
    /// entity for this context is despawned.
    pub fn on_cleanup(&mut self, cleanup: impl FnOnce(&mut DeferredWorld) + Send + Sync + 'static) {
        self.tracking.borrow_mut().add_cleanup(cleanup);
    }
}

impl<'p, 'w> ReadMutable for Cx<'p, 'w> {
    fn read_mutable<T>(&self, mutable: &Mutable<T>) -> T
    where
        T: Send + Sync + Copy + 'static,
    {
        self.tracking
            .borrow_mut()
            .track_component_id(mutable.cell, mutable.component, true);
        self.world.read_mutable(mutable)
    }

    fn read_mutable_clone<T>(&self, mutable: &Mutable<T>) -> T
    where
        T: Send + Sync + Clone + 'static,
    {
        self.tracking
            .borrow_mut()
            .track_component_id(mutable.cell, mutable.component, true);
        self.world.read_mutable_clone(mutable)
    }

    fn read_mutable_as_ref<T>(&self, mutable: &Mutable<T>) -> &T
    where
        T: Send + Sync + 'static,
    {
        self.tracking
            .borrow_mut()
            .track_component_id(mutable.cell, mutable.component, true);
        self.world.read_mutable_as_ref(mutable)
    }

    fn read_mutable_map<T, U, F: Fn(&T) -> U>(&self, mutable: &Mutable<T>, f: F) -> U
    where
        T: Send + Sync + 'static,
    {
        self.tracking
            .borrow_mut()
            .track_component_id(mutable.cell, mutable.component, true);
        self.world.read_mutable_map(mutable, f)
    }
}

impl<'p, 'w> ReadDerived for Cx<'p, 'w> {
    fn read_derived<R>(&self, derived: &Derived<R>) -> R
    where
        R: Send + Sync + Copy + 'static,
    {
        self.world
            .read_derived_with_scope(derived.id, &mut self.tracking.borrow_mut())
    }

    fn read_derived_clone<R>(&self, derived: &Derived<R>) -> R
    where
        R: Send + Sync + Clone + 'static,
    {
        self.world
            .read_derived_clone_with_scope(derived.id, &mut self.tracking.borrow_mut())
    }

    fn read_derived_map<R, U, F: Fn(&R) -> U>(&self, derived: &Derived<R>, f: F) -> U
    where
        R: Send + Sync + 'static,
    {
        self.world
            .read_derived_map_with_scope(derived.id, &mut self.tracking.borrow_mut(), f)
    }
}

/// A [`Lens`] is a function or callable object which accepts a reactive context, and returns
/// a value distilled or summarized from that context.
pub trait Lens<D> {
    fn call(&self, cx: &Cx) -> D;
}

impl<D, F: Fn(&Cx) -> D> Lens<D> for F {
    fn call(&self, cx: &Cx) -> D {
        (self)(cx)
    }
}

impl<D: Copy + Send + Sync + 'static> Lens<D> for Signal<D> {
    fn call(&self, cx: &Cx) -> D {
        self.get(cx)
    }
}

/// Similar to [`EntityRef`], except that it also records accesses in the tracking scope.
pub struct EntityRefTracked<'a, 'p, 'w> {
    entity: EntityRef<'w>,
    cx: &'a Cx<'p, 'w>,
}

impl<'a, 'p, 'w> EntityRefTracked<'a, 'p, 'w> {
    /// Return a reference to the Component `C` on the given entity. Calling this function
    /// adds the component as a dependency of the current tracking scope.
    pub fn get<C: Component>(&self) -> Option<&C> {
        let comp = self.entity.get::<C>();
        self.cx.tracking.borrow_mut().track_component::<C>(
            self.entity.id(),
            self.cx.world,
            comp.is_some(),
        );
        comp
    }

    /// Returns true if the entity has the given component.
    pub fn contains<C: Component>(&self) -> bool {
        let present = self.entity.contains::<C>();
        self.cx.tracking.borrow_mut().track_component::<C>(
            self.entity.id(),
            self.cx.world,
            present,
        );
        present
    }
}
