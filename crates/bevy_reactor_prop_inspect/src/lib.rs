mod attribute;
mod default_factory;
mod inspectable;
mod property_inspector;
mod registry;
mod types;

pub use attribute::*;
use bevy::{
    app::{App, Plugin},
    asset::embedded_asset,
};
pub use inspectable::*;
pub use property_inspector::property_inspector;
pub use registry::*;

use crate::default_factory::DefaultInspectorFactory;

pub struct PropertyInspectorPlugin;

impl Plugin for PropertyInspectorPlugin {
    fn build(&self, app: &mut App) {
        // Embedded icons
        embedded_asset!(app, "assets/icons/chevron-down.png");
        embedded_asset!(app, "assets/icons/chevron-right.png");
        embedded_asset!(app, "assets/icons/x.png");

        app.register_inspector::<DefaultInspectorFactory>();
        // .init_resource::<RecentColors>();
    }
}
