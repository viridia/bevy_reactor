//! Implementation of the reactive signals pattern for Bevy.
#![warn(missing_docs)]

use bevy::{
    app::{App, Plugin, Update},
    ecs::schedule::{IntoSystemConfigs, SystemSet},
};

mod callback;
mod cx;
mod derived;
mod mutable;
mod reaction;
mod signal;
mod tracking_scope;

pub use callback::Callback;
pub use cx::Cx;
pub use cx::Rcx;
pub use cx::RunContextRead;
pub use cx::RunContextSetup;
pub use cx::RunContextWrite;
pub use derived::Derived;
pub use derived::ReadDerived;
pub use mutable::Mutable;
pub use mutable::ReadMutable;
pub use mutable::WriteMutable;
pub use reaction::*;
pub use signal::IntoSignal;
pub use signal::Signal;
use tracking_scope::run_reactions;
pub use tracking_scope::DespawnScopes;
pub use tracking_scope::TrackingScope;
pub use tracking_scope::TrackingScopeTracing;

/// Plugin that adds the reactive UI system to the app.
pub struct SignalsPlugin;

/// A system set that runs all the reactions.
#[derive(SystemSet, Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReactionSet;

impl Plugin for SignalsPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Update, run_reactions.in_set(ReactionSet));
    }
}
