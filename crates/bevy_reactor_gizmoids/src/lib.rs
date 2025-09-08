//! Gizmoids: Gizmo-like dynamic meshes, but reatained and reactive.

mod gizmoid;
mod material;
mod mesh_builder;
mod shape_builder;

use crate::{
    gizmoid::sync_gizmoid_underlay,
    material::{OverlayMaterial, UnderlayMaterial},
};
use bevy::{
    app::{Plugin, PostUpdate},
    asset::embedded_asset,
    pbr::MaterialPlugin,
};
pub use gizmoid::{OverlayColor, gizmoid};
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
        app.add_systems(PostUpdate, sync_gizmoid_underlay);
    }
}
