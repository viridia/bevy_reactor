mod conditional;
mod cx;
mod derived;
pub mod effect;
mod foreach;
mod lcs;
mod mutable;
pub mod owner;
pub mod reaction;
mod signal;
mod tracking_scope;

use bevy::{
    app::{App, Plugin, Update},
    ecs::{hierarchy::Children, system::EntityCommands},
    scene2::{SceneList, SpawnRelatedScenes as _},
};
pub use conditional::*;
pub use cx::Cx;
pub use derived::*;
pub use foreach::*;
pub use mutable::*;
pub use signal::*;
pub use tracking_scope::*;

/// Plugin that adds the reactive UI system to the app.
pub struct ReactorPlugin;

impl Plugin for ReactorPlugin {
    fn build(&self, app: &mut App) {
        cleanup_tracking_scopes(app.world_mut());
        app.add_systems(Update, run_reactions);
    }
}

/// Trait that represents a function that can produce a [`SceneList`]. Used for conditional
/// blocks in control-flow templates.
pub trait SceneListFn: Send + Sync {
    fn spawn(&self, parent: EntityCommands);
}

impl<S: SceneList, F: Fn() -> S + Send + Sync + 'static> SceneListFn for F {
    fn spawn(&self, parent: EntityCommands) {
        parent.spawn_related_scenes::<Children>((self)());
    }
}
