mod attribute;
mod default_factory;
mod inspectable;
mod property_inspector;
mod registry;
mod types;

pub use attribute::*;
use bevy::app::{App, Plugin};
pub use inspectable::*;
pub use property_inspector::property_inspector;
pub use registry::*;

use crate::default_factory::DefaultInspectorFactory;

pub struct PropertyInspectorPlugin;

impl Plugin for PropertyInspectorPlugin {
    fn build(&self, app: &mut App) {
        app.register_inspector::<DefaultInspectorFactory>();
        // .init_resource::<RecentColors>();
    }
}
