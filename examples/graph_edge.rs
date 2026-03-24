//! Example which demonstrates drawing of node paths using the SDF shader.

use bevy::{
    prelude::*,
    render::storage::ShaderBuffer,
    ui::ComputedUiRenderTargetInfo,
    ui_render::prelude::MaterialNode,
};
use bevy_reactor_nodegraph::{DrawPathMaterial, DrawablePath, ReactorNodeGraphPlugin};

fn main() {
    App::new()
        .add_plugins((
            DefaultPlugins.set(ImagePlugin::default_nearest()),
            ReactorNodeGraphPlugin,
        ))
        .add_systems(Startup, setup_view_root)
        .add_systems(PostUpdate, update_scale)
        .add_systems(Update, close_on_esc)
        .run();
}

fn setup_view_root(
    mut commands: Commands,
    mut materials: ResMut<Assets<DrawPathMaterial>>,
    mut buffers: ResMut<Assets<ShaderBuffer>>,
) {
    commands.spawn((Camera::default(), Camera2d));

    // Path 1: S-curve using quadratic beziers (green)
    let mut path1 = DrawablePath::new(Srgba::new(0.2, 0.8, 0.4, 1.0), 3.0);
    path1.move_to(Vec2::new(10.0, 10.0));
    path1.quadratic_to(Vec2::new(90.0, 10.0), Vec2::new(90.0, 70.0));
    path1.quadratic_to(Vec2::new(90.0, 130.0), Vec2::new(10.0, 130.0));

    spawn_path(
        &mut commands, &mut materials, &mut buffers,
        &path1, Vec2::new(50.0, 30.0),
    );

    // Path 2: Straight line segments (red)

    let mut path2 = DrawablePath::new(Srgba::new(0.9, 0.2, 0.2, 1.0), 2.0);
    path2.move_to(Vec2::new(0.0, 0.0));
    path2.line_to(Vec2::new(100.0, 50.0));
    path2.line_to(Vec2::new(200.0, 0.0));
    path2.line_to(Vec2::new(300.0, 50.0));

    spawn_path(
        &mut commands, &mut materials, &mut buffers,
        &path2, Vec2::new(50.0, 200.0),
    );

    // Path 3: Connection-style curve (blue) - mimics node graph connection
    let src = Vec2::new(10.0, 50.0);
    let dst = Vec2::new(250.0, 120.0);
    let dx = (dst.x - src.x).abs().mul_add(0.0, (dst.x - src.x).abs() * 0.3).min(20.);
    let src1 = src + Vec2::new(dx, 0.);
    let dst1 = dst - Vec2::new(dx, 0.);

    let mut path3 = DrawablePath::new(Srgba::new(0.3, 0.5, 1.0, 1.0), 2.5);
    path3.move_to(src);

    let mlen = src1.distance(dst1);
    if mlen > 40. {
        let src2 = src1.lerp(dst1, 20. / mlen);
        let dst2 = src1.lerp(dst1, (mlen - 20.) / mlen);
        path3.quadratic_to(src1, src2);
        path3.line_to(dst2);
        path3.quadratic_to(dst1, dst);
    } else {
        let mid = src1.lerp(dst1, 0.5);
        path3.quadratic_to(src1, mid);
        path3.quadratic_to(dst1, dst);
    }

    spawn_path(
        &mut commands, &mut materials, &mut buffers,
        &path3, Vec2::new(50.0, 350.0),
    );

    // Path 4: Tight quadratic curve (yellow)
    let mut path4 = DrawablePath::new(Srgba::new(1.0, 0.8, 0.1, 1.0), 2.0);
    path4.move_to(Vec2::new(0.0, 80.0));
    path4.quadratic_to(Vec2::new(60.0, 0.0), Vec2::new(120.0, 80.0));
    path4.quadratic_to(Vec2::new(180.0, 160.0), Vec2::new(240.0, 80.0));

    spawn_path(
        &mut commands, &mut materials, &mut buffers,
        &path4, Vec2::new(50.0, 530.0),
    );
}

fn spawn_path(
    commands: &mut Commands,
    materials: &mut Assets<DrawPathMaterial>,
    buffers: &mut Assets<ShaderBuffer>,
    path: &DrawablePath,
    offset: Vec2,
) {
    let bounds = path.bounds();

    let buffer = buffers.add(ShaderBuffer::default());
    let mut material = DrawPathMaterial::new(buffer);
    material.update(path, buffers);

    commands.spawn((
        Node {
            position_type: PositionType::Absolute,
            left: Val::Px(offset.x + bounds.min.x),
            top: Val::Px(offset.y + bounds.min.y),
            width: Val::Px(bounds.width()),
            height: Val::Px(bounds.height()),
            ..default()
        },
        MaterialNode(materials.add(material)),
    ));
}

fn update_scale(
    q_node: Query<(&ComputedUiRenderTargetInfo, &MaterialNode<DrawPathMaterial>)>,
    mut materials: ResMut<Assets<DrawPathMaterial>>,
) {
    for (render_target, material_node) in q_node.iter() {
        if let Some(mut material) = materials.get_mut(material_node) {
            material.scale = 1.0 / render_target.scale_factor();
        }
    }
}

pub fn close_on_esc(input: Res<ButtonInput<KeyCode>>, mut exit: MessageWriter<AppExit>) {
    if input.just_pressed(KeyCode::Escape) {
        exit.write(AppExit::Success);
    }
}
