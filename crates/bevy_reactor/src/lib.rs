mod cx;
mod derived;
pub mod effect;
mod mutable;
pub mod owner;
pub mod reaction;
mod signal;
mod switch;
mod tracking_scope;

use bevy::app::{App, Plugin, Update};
pub use cx::Cx;
pub use derived::*;
pub use mutable::*;
pub use signal::*;
pub use switch::*;
pub use tracking_scope::*;

/// Plugin that adds the reactive UI system to the app.
pub struct ReactorPlugin;

impl Plugin for ReactorPlugin {
    fn build(&self, app: &mut App) {
        cleanup_tracking_scopes(app.world_mut());
        app.add_systems(Update, run_reactions);
    }
}
