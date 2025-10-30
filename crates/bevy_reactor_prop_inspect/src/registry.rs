use std::sync::Arc;

use bevy::prelude::*;

use crate::Inspectable;

/// Trait that defines a factory for creating inspectors. Multiple factories can be registered,
/// and the first one that returns true will be used to create the inspector.
pub trait InspectorFactory: Sync + Send {
    /// Examine the reflect data and decide what kind of widget to create to edit the
    /// data. Can return false if the data is not in a supported format.
    fn spawn_inspector(
        &self,
        world: &mut World,
        parent: Entity,
        inspectable: Arc<Inspectable>,
    ) -> bool;
}

#[derive(Resource, Default)]
pub struct InspectorFactoryRegistry(pub Vec<Box<dyn InspectorFactory>>);

impl InspectorFactoryRegistry {
    pub fn spawn_inspector(
        &self,
        world: &mut World,
        parent: Entity,
        inspectable: Arc<Inspectable>,
    ) -> bool {
        for factory in self.0.iter() {
            if factory.spawn_inspector(world, parent, inspectable.clone()) {
                return true;
            }
        }
        false
    }
}

pub trait RegisterInspectorFactory {
    fn register_inspector<T: InspectorFactory + Default + 'static>(&mut self) -> &mut Self;
}

impl RegisterInspectorFactory for App {
    fn register_inspector<T: InspectorFactory + Default + 'static>(&mut self) -> &mut Self {
        match self
            .world_mut()
            .get_resource_mut::<InspectorFactoryRegistry>()
        {
            Some(mut registry) => {
                registry.0.push(Box::<T>::default());
            }
            None => {
                self.world_mut()
                    .insert_resource(InspectorFactoryRegistry(vec![Box::<T>::default()]));
            }
        }
        self
    }
}
