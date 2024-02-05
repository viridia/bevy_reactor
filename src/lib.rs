//! A fine-grained reactive framework for Bevy.

#![warn(missing_docs)]

mod callback;
mod cond;
mod cx;
mod derived;
mod element;
mod element_effect;
mod r#for;
mod for_each;
mod for_index;
mod fragment;
mod hover;
mod lcs;
mod mutable;
mod node_span;
mod plugin;
mod presenter;
mod reaction;
mod signal;
// mod style;
mod style;
mod text;
mod tracking_scope;
mod view;
mod view_tuple;

pub use callback::CallDeferred;
pub use callback::Callback;
pub use cond::cond;
pub use cond::Cond;
pub use cx::Cx;
pub use cx::Rcx;
pub use cx::RunContextRead;
pub use cx::RunContextSetup;
pub use cx::RunContextWrite;
pub use derived::Derived;
pub use derived::ReadDerived;
pub use element::Element;
pub use element_effect::ElementEffect;
pub use for_each::ForEach;
pub use for_index::ForIndex;
pub use fragment::Fragment;
pub use hover::CreateHoverSignal;
pub use mutable::Mutable;
pub use mutable::ReadMutable;
pub use mutable::WriteMutable;
pub use plugin::ReactorPlugin;
pub use presenter::*;
pub use r#for::For;
pub use reaction::*;
pub use signal::Signal;
pub use signal::SignalClone;
pub use style::StyleBuilder;
pub use style::StyleBuilderBackground;
pub use style::StyleBuilderBorderColor;
pub use style::StyleBuilderFont;
pub use style::StyleBuilderLayout;
pub use style::StyleBuilderOutline;
pub use style::StyleBuilderPointerEvents;
pub use style::StyleBuilderTextureAtlas;
pub use style::StyleBuilderZIndex;
pub use style::StyleHandle;
pub use style::StyleTuple;
pub use style::WithStyles;
pub use text::*;
pub(crate) use tracking_scope::DespawnScopes;
pub(crate) use tracking_scope::TrackingScope;
pub use view::*;
pub use view_tuple::ViewTuple;
