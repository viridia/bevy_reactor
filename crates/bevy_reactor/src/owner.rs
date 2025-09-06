use bevy::prelude::*;

/// A relationship component used to define a hidden satellite entity that is lifetime
/// scoped to its owning entity.
#[derive(Component, Clone, PartialEq, Eq, Debug)]
#[relationship(relationship_target = OwnerOf)]
pub struct OwnedBy(pub Entity);

impl OwnedBy {
    pub fn get(&self) -> Entity {
        self.0
    }
}

impl Default for OwnedBy {
    fn default() -> Self {
        OwnedBy(Entity::PLACEHOLDER)
    }
}

/// The set of entities owned by this one, see [`OwnedBy`].
#[derive(Component, Default)]
#[relationship_target(relationship = OwnedBy, linked_spawn)]
pub struct OwnerOf(Vec<Entity>);

impl core::ops::Deref for OwnerOf {
    type Target = [Entity];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
