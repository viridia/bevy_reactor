//! Gizmoids: Gizmo-like dynamic meshes, but reatained and reactive.

mod gizmoid;
mod line3_builder;
mod material;
mod mesh_builder;
mod shape_builder;

use crate::material::{OverlayMaterial, UnderlayMaterial};
use bevy::{
    app::{Plugin, PostUpdate},
    asset::embedded_asset,
    pbr::MaterialPlugin,
};
pub use gizmoid::gizmoid;
pub use line3_builder::Line3dBuilder;
pub use material::OverlayColor;
pub use mesh_builder::MeshBuilder;
pub use shape_builder::{DrawPlane, PolygonOptions, ShapeBuilder, StrokeMarker};

/// Plugin for the overlays module.
pub struct GizmoidsPlugin;

impl Plugin for GizmoidsPlugin {
    fn build(&self, app: &mut bevy::app::App) {
        embedded_asset!(app, "assets/overlay.wgsl");
        app.add_plugins((
            MaterialPlugin::<OverlayMaterial>::default(),
            MaterialPlugin::<UnderlayMaterial>::default(),
        ));
        app.add_systems(PostUpdate, material::sync_underlay);
    }
}
