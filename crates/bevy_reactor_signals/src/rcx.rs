use std::cell::RefCell;

use bevy::{
    ecs::world::DeferredWorld,
    prelude::{Component, Entity, Parent, Resource, World},
};

use crate::{
    derived::ReadDerivedInternal, Derived, Mutable, ReadDerived, ReadMutable, TrackingScope,
};

/// Immutable reactive context, used for reactive closures such as derived signals.
/// This is a stripped down version of [`Cx`] that does not allow creating new reactions,
/// and which has no parameters.
pub struct Rcx<'p, 'w> {
    /// Bevy World
    pub(crate) world: &'w World,

    /// The entity that owns the tracking scope (or will own it).
    pub(crate) owner: Entity,

    /// Set of reactive resources referenced by the presenter.
    pub(crate) tracking: RefCell<&'p mut TrackingScope>,
}

impl<'p, 'w> Rcx<'p, 'w> {
    /// Create a new read-only reactive context.
    pub fn new(world: &'w World, owner: Entity, tracking: &'p mut TrackingScope) -> Self {
        Self {
            world,
            owner,
            tracking: RefCell::new(tracking),
        }
    }

    /// Access to immutable world from reactive context.
    pub fn world(&self) -> &World {
        self.world
    }

    /// Return a reference to the resource of the given type. Calling this function
    /// adds the resource as a dependency of the current tracking scope.
    pub fn read_resource<T: Resource>(&self) -> &T {
        self.tracking.borrow_mut().track_resource::<T>(self.world);
        self.world.resource::<T>()
    }

    /// Return a reference to the Component `C` on the given entity. Calling this function
    /// adds the component as a dependency of the current tracking scope.
    pub fn read_component<C: Component>(&self, entity: Entity) -> Option<&C> {
        self.tracking
            .borrow_mut()
            .track_component::<C>(entity, self.world);
        self.world.entity(entity).get::<C>()
    }

    /// Return a reference to the Component `C` on the owner entity of the current
    /// context, or one of it's ancestors. This searches up the entity tree until it finds
    /// a component of the given type.
    pub fn use_inherited_component<C: Component>(&self) -> Option<&C> {
        let mut entity = self.owner;
        loop {
            let ec = self.read_component(entity);
            if ec.is_some() {
                return ec;
            }
            match self.world.entity(entity).get::<Parent>() {
                Some(parent) => entity = **parent,
                _ => return None,
            }
        }
    }

    /// Indicate that we want to consider the current tracking scope out of date at the next
    /// inter-system interval.
    pub fn set_deferred_change(&self) {
        self.tracking.borrow_mut().set_deferred_change();
    }

    /// Add a cleanup function which is run once before the next reaction, or when the owner
    /// entity for this context is despawned.
    pub fn on_cleanup(&mut self, cleanup: impl FnOnce(&mut DeferredWorld) + Send + Sync + 'static) {
        self.tracking.borrow_mut().add_cleanup(cleanup);
    }
}

impl<'p, 'w> ReadMutable for Rcx<'p, 'w> {
    fn read_mutable<T>(&self, mutable: &Mutable<T>) -> T
    where
        T: Send + Sync + Copy + 'static,
    {
        self.tracking
            .borrow_mut()
            .track_component_id(mutable.cell, mutable.component);
        self.world.read_mutable(mutable)
    }

    fn read_mutable_clone<T>(&self, mutable: &Mutable<T>) -> T
    where
        T: Send + Sync + Clone + 'static,
    {
        self.tracking
            .borrow_mut()
            .track_component_id(mutable.cell, mutable.component);
        self.world.read_mutable_clone(mutable)
    }

    fn read_mutable_as_ref<T>(&self, mutable: &Mutable<T>) -> &T
    where
        T: Send + Sync + 'static,
    {
        self.tracking
            .borrow_mut()
            .track_component_id(mutable.cell, mutable.component);
        self.world.read_mutable_as_ref(mutable)
    }

    fn read_mutable_map<T, U, F: Fn(&T) -> U>(&self, mutable: &Mutable<T>, f: F) -> U
    where
        T: Send + Sync + 'static,
    {
        self.tracking
            .borrow_mut()
            .track_component_id(mutable.cell, mutable.component);
        self.world.read_mutable_map(mutable, f)
    }
}

impl<'p, 'w> ReadDerived for Rcx<'p, 'w> {
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
