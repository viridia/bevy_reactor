//! Example which demonstrates drawing of node paths.

use bevy::{
    prelude::*,
    scene2::{CommandsSpawnScene, bsn},
};
use bevy_reactor::ReactorPlugin;
use bevy_reactor_nodegraph::ReactorNodeGraphPlugin;

fn main() {
    App::new()
        .add_plugins((
            DefaultPlugins.set(ImagePlugin::default_nearest()),
            ReactorPlugin,
            ReactorNodeGraphPlugin,
        ))
        .add_systems(Startup, setup_view_root)
        .add_systems(Update, close_on_esc)
        .run();
}

// const DOT_IMAGE: &str = "embedded://bevy_reactor_nodegraph/assets/dot.png";

fn setup_view_root(asset_server: Res<AssetServer>, mut commands: Commands) {
    let image = asset_server.load("embedded://bevy_reactor_nodegraph/assets/dot.png");

    commands.spawn((Camera::default(), Camera2d));
    commands.spawn_scene(bsn!(
        (Node {
            position_type: PositionType::Absolute,
            left: px(0),
            right: px(0),
            top: px(0),
            bottom: px(0),
        }
        ImageNode {
            color: Color::srgba(0.2, 0.2, 0.4, 1.0),
            image: {image.clone()},
            image_mode: NodeImageMode::Tiled {
                tile_x: true,
                tile_y: true,
                stretch_value: 0.5,
            },
        }
        BackgroundColor(Srgba::new(0.1, 0.1, 0.2, 1.0))
        // [Connection {
        //     src_pos: Vec2::new(10.0, 10.0),
        //     dst_pos: Vec2::new(180.0, 135.0),
        //     color: Color::srgb(1.0, 0.0, 0.0),
        // }]
        )
    ));
}

pub fn close_on_esc(input: Res<ButtonInput<KeyCode>>, mut exit: MessageWriter<AppExit>) {
    if input.just_pressed(KeyCode::Escape) {
        exit.write(AppExit::Success);
    }
}
