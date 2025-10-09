use bevy::{app::Plugin, asset::embedded_asset, ui_render::UiMaterialPlugin};

mod display;
mod draw_path;

pub use display::EdgeDisplay;
pub use draw_path::{DrawPathMaterial, DrawablePath, DrawablePathSegment};

use display::on_insert_edge;

// use crate::display::on_add_edge;

pub struct ReactorNodeGraphPlugin;

impl Plugin for ReactorNodeGraphPlugin {
    fn build(&self, app: &mut bevy::app::App) {
        embedded_asset!(app, "assets/draw_path.wgsl");

        app.add_plugins(UiMaterialPlugin::<DrawPathMaterial>::default());
        // app.add_observer(on_add_edge);
        app.add_observer(on_insert_edge);
    }
}
