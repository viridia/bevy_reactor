use std::f32::consts::{PI, TAU};

use bevy::{
    asset::RenderAssetUsages,
    color::palettes::{basic::SILVER, css},
    prelude::*,
    render::render_resource::{Extent3d, TextureDimension, TextureFormat},
    scene2::{CommandsSpawnScene, bsn},
};
use bevy_reactor::{Cx, ReactorPlugin, effect};
use bevy_reactor_gizmoids::{
    DrawPlane, GizmoidsPlugin, Line3dBuilder, OverlayColor, PolygonOptions, ShapeBuilder, gizmoid,
};

fn main() {
    App::new()
        .add_plugins((
            DefaultPlugins.set(ImagePlugin::default_nearest()),
            ReactorPlugin,
            GizmoidsPlugin,
        ))
        .add_systems(Startup, setup)
        .add_systems(Update, rotate)
        .run();
}

/// A marker component for our shapes so we can query them separately from the ground plane
#[derive(Component)]
struct Shape;

const SHAPES_X_EXTENT: f32 = 14.0;
const Z_EXTENT: f32 = 5.0;

fn setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut images: ResMut<Assets<Image>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    let debug_material = materials.add(StandardMaterial {
        base_color_texture: Some(images.add(uv_debug_texture())),
        ..default()
    });

    let shapes = [
        meshes.add(Cuboid::default()),
        meshes.add(Tetrahedron::default()),
        meshes.add(Capsule3d::default()),
        meshes.add(Torus::default()),
        meshes.add(Cylinder::default()),
        meshes.add(Cone::default()),
        meshes.add(ConicalFrustum::default()),
        meshes.add(Sphere::default().mesh().ico(5).unwrap()),
        meshes.add(Sphere::default().mesh().uv(32, 18)),
    ];

    let num_shapes = shapes.len();

    for (i, shape) in shapes.into_iter().enumerate() {
        commands.spawn((
            Mesh3d(shape),
            MeshMaterial3d(debug_material.clone()),
            Transform::from_xyz(
                -SHAPES_X_EXTENT / 2. + i as f32 / (num_shapes - 1) as f32 * SHAPES_X_EXTENT,
                2.0,
                Z_EXTENT / 2.,
            )
            .with_rotation(Quat::from_rotation_x(-PI / 4.)),
            Shape,
        ));
    }

    commands.spawn_scene(bsn! {
        :gizmoid(|cx: &Cx, builder: &mut ShapeBuilder| {
            let time = cx.resource::<Time>().elapsed_secs().rem_euclid(TAU);
            let size = time.sin() * 2.0 + 4.0;
            let mut vertices: Vec<Vec2> = Vec::with_capacity(14);
            for i in 0..7 {
                let angle = TAU * (i as f32) / 7.0;
                vertices.push(Vec2::new(angle.sin() * size, angle.cos() * size));
                let angle2 = angle + TAU / 14.0;
                vertices.push(Vec2::new(angle2.sin() * 3.0, angle2.cos() * 3.0));
            }
            builder
                .with_stroke_width(0.3)
                .with_draw_plane(DrawPlane::XZ)
                .with_offset(0.1)
                .stroke_polygon(&vertices, PolygonOptions {
                    closed: true,
                    // dash_length: 0.5,
                    // gap_length: 0.1,
                    ..default()
                });
        })
        OverlayColor {
            base: Color::srgba(1.0, 1.0, 0.0, 0.7),
            underlay: 0.15,
        }

        // Transform rotation animation
        effect::effect(|cx: &Cx| {
            // `Time` resource is always changed.
            (cx.resource::<Time>().elapsed_secs() * 0.27).rem_euclid(TAU)
        }, |entity, angle| {
            if let Some(mut transform) = entity.get_mut::<Transform>() {
                transform.rotation = Quat::from_axis_angle(Vec3::Y, angle);
            }
        })
    });

    commands.spawn_scene(bsn! {
        :gizmoid(|cx: &Cx, builder: &mut ShapeBuilder| {
            let time = cx.resource::<Time>().elapsed_secs().rem_euclid(TAU);
            let size = time.sin() * 2.0 + 4.0;
            builder
                .with_stroke_width(0.5)
                .stroke_rect(Rect::new(-size, -size, size, size));
        })
        OverlayColor {
            base: {css::LIME.with_alpha(0.7)},
            underlay: 0.15,
        }

        // Transform rotation animation
        effect::effect(|cx: &Cx| {
            // `Time` resource is always changed.
            (cx.resource::<Time>().elapsed_secs() * 0.5).rem_euclid(TAU)
        }, |entity, angle| {
            if let Some(mut transform) = entity.get_mut::<Transform>() {
                transform.rotation = Quat::from_axis_angle(Vec3::Y, angle);
            }
        })
    });

    commands.spawn_scene(bsn! {
        :gizmoid(|cx: &Cx, builder: &mut Line3dBuilder| {
            let time = cx.resource::<Time>().elapsed_secs().rem_euclid(TAU);
            let size = time.sin() * 2.0 + 4.0;
            builder.draw_cuboid(Vec3::ZERO, Cuboid::new(size, size, size));
        })
        OverlayColor {
            base: Color::srgba(0.1, 0.1, 0.0, 1.0),
            underlay: 0.3,
            alpha_mode: AlphaMode::Add,
        }

        // Transform rotation animation
        effect::effect(|cx: &Cx| {
            // `Time` resource is always changed.
            (cx.resource::<Time>().elapsed_secs() * 0.5).rem_euclid(TAU)
        }, |entity, angle| {
            if let Some(mut transform) = entity.get_mut::<Transform>() {
                transform.rotation = Quat::from_axis_angle(Vec3::Y, angle);
            }
        })
    });

    commands.spawn((
        PointLight {
            shadow_maps_enabled: true,
            intensity: 10_000_000.,
            range: 100.0,
            shadow_depth_bias: 0.2,
            ..default()
        },
        Transform::from_xyz(8.0, 16.0, 8.0),
    ));

    // ground plane
    commands.spawn((
        Mesh3d(meshes.add(Plane3d::default().mesh().size(50.0, 50.0).subdivisions(10))),
        MeshMaterial3d(materials.add(Color::from(SILVER))),
    ));

    commands.spawn((
        Camera3d::default(),
        Transform::from_xyz(0.0, 7., 14.0).looking_at(Vec3::new(0., 1., 0.), Vec3::Y),
    ));
}

fn rotate(mut query: Query<&mut Transform, With<Shape>>, time: Res<Time>) {
    for mut transform in &mut query {
        transform.rotate_y(time.delta_secs() / 2.);
    }
}

/// Creates a colorful test pattern
fn uv_debug_texture() -> Image {
    const TEXTURE_SIZE: usize = 8;

    let mut palette: [u8; 32] = [
        255, 102, 159, 255, 255, 159, 102, 255, 236, 255, 102, 255, 121, 255, 102, 255, 102, 255,
        198, 255, 102, 198, 255, 255, 121, 102, 255, 255, 236, 102, 255, 255,
    ];

    let mut texture_data = [0; TEXTURE_SIZE * TEXTURE_SIZE * 4];
    for y in 0..TEXTURE_SIZE {
        let offset = TEXTURE_SIZE * y * 4;
        texture_data[offset..(offset + TEXTURE_SIZE * 4)].copy_from_slice(&palette);
        palette.rotate_right(4);
    }

    Image::new_fill(
        Extent3d {
            width: TEXTURE_SIZE as u32,
            height: TEXTURE_SIZE as u32,
            depth_or_array_layers: 1,
        },
        TextureDimension::D2,
        &texture_data,
        TextureFormat::Rgba8UnormSrgb,
        RenderAssetUsages::RENDER_WORLD,
    )
}
