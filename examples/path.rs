//! Example which demonstrates drawing of node paths.

use bevy::{
    prelude::*,
    scene2::{CommandsSpawnScene, bsn},
};
use bevy_reactor::ReactorPlugin;
use bevy_reactor_nodegraph::{EdgeDisplay, ReactorNodeGraphPlugin};

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

fn setup_view_root(mut commands: Commands) {
    commands.spawn((Camera::default(), Camera2d));

    commands.spawn_scene(bsn!(EdgeDisplay {
        src_pos: Vec2::new(10.0, 10.0),
        dst_pos: Vec2::new(180.0, 135.0),
        color: Color::srgb(1.0, 0.0, 0.0),
    }));
}

pub fn close_on_esc(input: Res<ButtonInput<KeyCode>>, mut exit: MessageWriter<AppExit>) {
    if input.just_pressed(KeyCode::Escape) {
        exit.write(AppExit::Success);
    }
}
