use std::marker::PhantomData;

use crate::{owner::OwnedBy, signal::Signal};
use bevy::{
    ecs::{component::ComponentId, world::DeferredWorld},
    prelude::*,
};

/// Contains a mutable reactive value.
#[derive(Component)]
pub(crate) struct MutableCell<T>(pub(crate) T);

/// Contains a reference to a reactive mutable variable.
#[derive(PartialEq, Debug)]
pub struct Mutable<T> {
    /// The entity that holds the mutable value.
    pub(crate) cell: Entity,
    /// The component id for the mutable cell.
    pub(crate) component: ComponentId,

    /// Marker
    pub(crate) marker: std::marker::PhantomData<T>,
}

impl<T> Mutable<T> {
    /// The entity that holds the mutable value.
    pub fn id(&self) -> Entity {
        self.cell
    }
}

impl<T> Copy for Mutable<T> {}
impl<T> Clone for Mutable<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Mutable<T>
where
    T: Send + Sync + 'static,
{
    /// Update a mutable value in place using a callback. The callback is passed a
    /// `Mut<T>` which can be used to modify the value.
    pub fn update<W: WriteMutable, F: FnOnce(Mut<T>)>(&self, w: &mut W, updater: F) {
        w.update_mutable(self.id(), updater);
    }
}

impl<T> Mutable<T>
where
    T: PartialEq + Send + Sync + 'static,
{
    /// Returns a signal for this [`Mutable`] with Copy semantics.
    pub fn signal(&self) -> Signal<T> {
        Signal::Mutable(*self)
    }

    /// Get a reference to the value of this [`Mutable`].
    ///
    /// Arguments:
    /// * `cx`: The reactive context.
    pub fn as_ref<'a, 'b: 'a, R: ReadMutable>(&'a self, cx: &'b mut R) -> &'a T {
        cx.read_mutable_as_ref(self)
    }
}

impl<T> Mutable<T>
where
    T: PartialEq + Copy + Send + Sync + 'static,
{
    /// Get the value of this [`Mutable`] with Copy semantics.
    ///
    /// Arguments:
    /// * `cx`: The reactive context.
    pub fn get<R: ReadMutable>(&self, cx: &R) -> T {
        cx.read_mutable(self)
    }

    /// Set the value of this [`Mutable`] with Copy semantics.
    ///
    /// Arguments:
    /// * `cx`: The reactive context.
    /// * `value`: The new value.
    pub fn set<R: WriteMutable>(&self, cx: &mut R, value: T) {
        cx.write_mutable(self.cell, value);
    }
}

impl<T> Mutable<T>
where
    T: PartialEq + Clone + Send + Sync + 'static,
{
    /// Get the value of this [`Mutable`] with Clone semantics.
    ///
    /// Arguments:
    /// * `cx`: The reactive context.
    pub fn get_clone<R: ReadMutable>(&self, cx: &mut R) -> T {
        cx.read_mutable_clone(self)
    }

    /// Set the value of this [`Mutable`] with Clone semantics.
    ///
    /// Arguments:
    /// * `cx`: The reactive context.
    /// * `value`: The new value.
    pub fn set_clone<R: WriteMutable>(&self, cx: &mut R, value: T) {
        cx.write_mutable(self.cell, value);
    }
}

/// Function to create a mutable
pub fn create_mutable<T: Send + Sync + 'static>(
    world: &mut World,
    owner: Entity,
    init: T,
) -> Mutable<T> {
    let cell = world.spawn(MutableCell::<T>(init)).id();
    world.entity_mut(owner).add_one_related::<OwnedBy>(cell);
    let component = world.register_component::<MutableCell<T>>();
    Mutable {
        cell,
        component,
        marker: PhantomData,
    }
}

/// Trait for low-level read-access to mutables given an entity id.
pub trait ReadMutable {
    /// Read the value of a mutable variable using Copy semantics. Calling this function adds the
    /// mutable to the current tracking scope.
    fn read_mutable<T>(&self, mutable: &Mutable<T>) -> T
    where
        T: Send + Sync + Copy + 'static;

    /// Read the value of a mutable variable using Clone semantics. Calling this function adds the
    /// mutable to the current tracking scope.
    fn read_mutable_clone<T>(&self, mutable: &Mutable<T>) -> T
    where
        T: Send + Sync + Clone + 'static;

    /// Return an immutable reference to the mutable variable.
    fn read_mutable_as_ref<T>(&self, mutable: &Mutable<T>) -> &T
    where
        T: Send + Sync + 'static;

    /// Read the value of a mutable variable using a mapping function.
    fn read_mutable_map<T, U, F: Fn(&T) -> U>(&self, mutable: &Mutable<T>, f: F) -> U
    where
        T: Send + Sync + 'static;
}

/// Trait for low-level write-access to mutables given an entity id.
pub trait WriteMutable {
    /// Write the value of a mutable variable using Copy semantics. Does nothing if
    /// the value being set matches the existing value.
    fn write_mutable<T>(&mut self, mutable: Entity, value: T)
    where
        T: Send + Sync + PartialEq + 'static;

    /// Update a mutable value in place using a callback. The callback is passed a
    /// `Mut<T>` which can be used to modify the value.
    fn update_mutable<T, F: FnOnce(Mut<T>)>(&mut self, mutable: Entity, updater: F)
    where
        T: Send + Sync + 'static;
}

/// Trait for creating new mutable variables.
pub trait CreateMutable {
    /// Create a new [`Mutable`].
    fn create_mutable<T>(&mut self, init: T) -> Mutable<T>
    where
        T: Send + Sync + 'static;
}

// /// Custom command which updates the state of a mutable cell.
// /// (Not used yet, waiting on changes in Bevy 0.14)
// pub(crate) struct UpdateMutableCell<T> {
//     pub(crate) mutable: Entity,
//     pub(crate) value: T,
// }

// impl<T: Send + Sync + 'static + PartialEq> Command for UpdateMutableCell<T> {
//     fn apply(self, world: &mut World) {
//         let mut mutable_ent = world.entity_mut(self.mutable);
//         let mut mutable = mutable_ent.get_mut::<MutableCell<T>>().unwrap();
//         if mutable.0 != self.value {
//             mutable.0 = self.value;
//         }
//     }
// }

impl ReadMutable for World {
    fn read_mutable<T>(&self, mutable: &Mutable<T>) -> T
    where
        T: Send + Sync + Copy + 'static,
    {
        let mutable_entity = self.entity(mutable.cell);
        mutable_entity.get::<MutableCell<T>>().unwrap().0
    }

    fn read_mutable_clone<T>(&self, mutable: &Mutable<T>) -> T
    where
        T: Send + Sync + Clone + 'static,
    {
        let mutable_entity = self.entity(mutable.cell);
        mutable_entity.get::<MutableCell<T>>().unwrap().0.clone()
    }

    fn read_mutable_as_ref<T>(&self, mutable: &Mutable<T>) -> &T
    where
        T: Send + Sync + 'static,
    {
        let mutable_entity = self.entity(mutable.cell);
        &mutable_entity.get::<MutableCell<T>>().unwrap().0
    }

    fn read_mutable_map<T, U, F: Fn(&T) -> U>(&self, mutable: &Mutable<T>, f: F) -> U
    where
        T: Send + Sync + 'static,
    {
        let mutable_entity = self.entity(mutable.cell);
        f(&mutable_entity.get::<MutableCell<T>>().unwrap().0)
    }
}

impl WriteMutable for World {
    /// Write the value of a mutable variable. Does nothing if the value being set matches the
    /// existing value.
    fn write_mutable<T>(&mut self, mutable: Entity, value: T)
    where
        T: Send + Sync + PartialEq + 'static,
    {
        let mut entt = self.entity_mut(mutable);
        let mut cell = entt.get_mut::<MutableCell<T>>().unwrap();
        if cell.0 != value {
            cell.0 = value;
        }
    }

    fn update_mutable<T, F: FnOnce(Mut<T>)>(&mut self, mutable: Entity, updater: F)
    where
        T: Send + Sync + 'static,
    {
        let value = self.get_mut::<MutableCell<T>>(mutable).unwrap();
        let inner = value.map_unchanged(|v| &mut v.0);
        (updater)(inner);
    }
}

impl CreateMutable for World {
    fn create_mutable<T>(&mut self, init: T) -> Mutable<T>
    where
        T: Send + Sync + 'static,
    {
        let cell = self.spawn(MutableCell::<T>(init)).id();
        let component = self.register_component::<MutableCell<T>>();
        Mutable {
            cell,
            component,
            marker: PhantomData,
        }
    }
}

impl<'w> ReadMutable for DeferredWorld<'w> {
    fn read_mutable<T>(&self, mutable: &Mutable<T>) -> T
    where
        T: Send + Sync + Copy + 'static,
    {
        let mutable_entity = self.entity(mutable.cell);
        mutable_entity.get::<MutableCell<T>>().unwrap().0
    }

    fn read_mutable_clone<T>(&self, mutable: &Mutable<T>) -> T
    where
        T: Send + Sync + Clone + 'static,
    {
        let mutable_entity = self.entity(mutable.cell);
        mutable_entity.get::<MutableCell<T>>().unwrap().0.clone()
    }

    fn read_mutable_as_ref<T>(&self, mutable: &Mutable<T>) -> &T
    where
        T: Send + Sync + 'static,
    {
        let mutable_entity = self.entity(mutable.cell);
        &mutable_entity.get::<MutableCell<T>>().unwrap().0
    }

    fn read_mutable_map<T, U, F: Fn(&T) -> U>(&self, mutable: &Mutable<T>, f: F) -> U
    where
        T: Send + Sync + 'static,
    {
        let mutable_entity = self.entity(mutable.cell);
        f(&mutable_entity.get::<MutableCell<T>>().unwrap().0)
    }
}

impl<'w> WriteMutable for DeferredWorld<'w> {
    /// Write the value of a mutable variable. Does nothing if the value being set matches the
    /// existing value.
    fn write_mutable<T>(&mut self, mutable: Entity, value: T)
    where
        T: Send + Sync + PartialEq + 'static,
    {
        let mut entt = self.entity_mut(mutable);
        let mut cell = entt.get_mut::<MutableCell<T>>().unwrap();
        if cell.0 != value {
            cell.0 = value;
        }
    }

    fn update_mutable<T, F: FnOnce(Mut<T>)>(&mut self, mutable: Entity, updater: F)
    where
        T: Send + Sync + 'static,
    {
        let value = self.get_mut::<MutableCell<T>>(mutable).unwrap();
        let inner = value.map_unchanged(|v| &mut v.0);
        (updater)(inner);
    }
}

#[cfg(test)]
mod tests {
    use crate::{Cx, TrackingScope};

    use super::*;

    #[test]
    fn test_mutable_copy() {
        let mut world = World::default();
        let mut scope = TrackingScope::new(world.change_tick());
        let owner = world.spawn_empty().id();

        let mutable = world.create_mutable::<i32>(0);
        let reader = mutable.signal();
        let reader2 = world.create_mutable::<i32>(0).signal();
        let cx = Cx::new(&world, owner, &mut scope);

        // Check initial values
        assert_eq!(reader.get(&cx), 0);
        assert_eq!(reader2.get(&cx), 0);

        // Update signals
        mutable.set(&mut world, 1);
        let cx = Cx::new(&world, owner, &mut scope);

        assert_eq!(reader.get(&cx), 1);
        assert_eq!(reader2.get(&cx), 0);
    }

    #[test]
    fn test_mutable_clone() {
        let mut world = World::default();
        let mut scope = TrackingScope::new(world.change_tick());
        let owner = world.spawn_empty().id();

        let mutable = world.create_mutable("Hello".to_string());
        let reader = mutable.signal();
        let reader2 = world.create_mutable::<i32>(0).signal();
        let cx = Cx::new(&world, owner, &mut scope);

        // Check initial values
        assert_eq!(reader.get_clone(&cx), "Hello".to_string());
        assert_eq!(reader2.get(&cx), 0);

        // Update signals
        mutable.set_clone(&mut world, "Goodbye".to_string());

        // Signals should have changed
        let cx = Cx::new(&world, owner, &mut scope);
        assert_eq!(reader.get_clone(&cx), "Goodbye".to_string());
        assert_eq!(reader2.get(&cx), 0);
    }
}
