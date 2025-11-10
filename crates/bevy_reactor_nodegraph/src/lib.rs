use bevy::{
    app::{Plugin, PostUpdate},
    asset::embedded_asset,
    ecs::schedule::IntoScheduleConfigs,
    ui::UiSystems,
    ui_render::UiMaterialPlugin,
};

mod draw_path;
mod gesture;
mod graph;

pub use draw_path::*;
pub use gesture::*;
pub use graph::*;

use crate::graph::update_edge_shader;

pub struct ReactorNodeGraphPlugin;

impl Plugin for ReactorNodeGraphPlugin {
    fn build(&self, app: &mut bevy::app::App) {
        embedded_asset!(app, "assets/draw_path.wgsl");
        embedded_asset!(app, "assets/dot.png");

        app.init_resource::<GestureState>();
        app.add_plugins(UiMaterialPlugin::<DrawPathMaterial>::default());
        app.add_observer(on_insert_edge);
        app.add_systems(
            PostUpdate,
            (
                update_edge_shader.after(UiSystems::Stack),
                update_node_outlines,
                update_terminal_positions,
            ),
        );
    }
}
