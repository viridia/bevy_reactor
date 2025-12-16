//! Example which uses states and a switch view.

use std::{
    any::TypeId,
    ops::Range,
    sync::{Arc, Mutex},
};

use bevy::{color::palettes::css, ecs::component::Tick, prelude::*, reflect::Typed as _};
use bevy_reactor::{ReactorPlugin, TrackingScope};
use bevy_reactor_formulae::{
    ExprType, HostState, Param, ScriptModuleAsset, ScriptModuleLoader, VM,
    vm::{InvocationContext, StackValue, VMError},
};
use smol_str::SmolStr;

fn main() {
    let mut host = HostState::new();
    host.add_host_type::<Color>("Color")
        .add_static_method(
            "srgb",
            color_srgb,
            vec![
                Param {
                    name: SmolStr::new_static("r"),
                    ty: ExprType::F32,
                },
                Param {
                    name: SmolStr::new_static("g"),
                    ty: ExprType::F32,
                },
                Param {
                    name: SmolStr::new_static("b"),
                    ty: ExprType::F32,
                },
            ],
            ExprType::Reflected(Color::type_info()),
        )
        .add_static_method(
            "srgba",
            color_srgba,
            vec![
                Param {
                    name: SmolStr::new_static("r"),
                    ty: ExprType::F32,
                },
                Param {
                    name: SmolStr::new_static("g"),
                    ty: ExprType::F32,
                },
                Param {
                    name: SmolStr::new_static("b"),
                    ty: ExprType::F32,
                },
                Param {
                    name: SmolStr::new_static("a"),
                    ty: ExprType::F32,
                },
            ],
            ExprType::Reflected(Color::type_info()),
        );
    host.add_global_prop("self", get_self, ExprType::Entity);
    host.add_global_prop("player", get_player, ExprType::Entity);
    host.get_host_type_mut::<Entity>().unwrap().add_property(
        "position",
        entity_position,
        ExprType::Reflected(Vec3::type_info()),
    );
    host.add_host_type::<Vec3>("Vec3").add_method(
        "distance",
        vec3_distance,
        vec![Param {
            name: SmolStr::new_static("rhs"),
            ty: ExprType::Reflected(Vec3::type_info()),
        }],
        ExprType::F32,
    );
    let host_arc = Arc::new(Mutex::new(host));

    App::new()
        .add_plugins((
            DefaultPlugins.set(ImagePlugin::default_nearest()),
            ReactorPlugin,
        ))
        .init_asset::<ScriptModuleAsset>()
        .insert_resource(ScriptHostState(host_arc.clone()))
        .register_asset_loader(ScriptModuleLoader { host: host_arc })
        .init_resource::<Random32>()
        .add_systems(Startup, setup)
        .add_systems(Update, close_on_esc)
        .add_systems(FixedUpdate, (move_player, run_script))
        .run();
}

#[derive(Component)]
struct Base;

#[derive(Component)]
struct Player(Vec3);

#[derive(Component)]
struct MainScript(pub Handle<ScriptModuleAsset>);

#[derive(Resource)]
struct ScriptHostState(pub Arc<Mutex<HostState>>);

fn setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    assets: Res<AssetServer>,
) {
    let script: Handle<ScriptModuleAsset> = assets.load("proximity.fmod");

    commands.spawn((
        Mesh3d(meshes.add(Sphere::default().mesh().ico(5).unwrap())),
        MeshMaterial3d(materials.add(StandardMaterial {
            base_color: css::MAGENTA.into(),
            ..default()
        })),
        Transform::from_xyz(0.0, 1.0, 0.0),
        Base,
        MainScript(script),
    ));

    commands.spawn((
        Mesh3d(meshes.add(Sphere::default().mesh().ico(5).unwrap())),
        MeshMaterial3d(materials.add(StandardMaterial {
            base_color: css::GRAY.into(),
            ..default()
        })),
        Transform::from_xyz(1.0, 1.0, 0.0),
        Player(Vec3::ZERO),
    ));

    commands.spawn((
        PointLight {
            shadows_enabled: true,
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
        MeshMaterial3d(materials.add(Color::from(css::SILVER))),
    ));

    commands.spawn((
        Camera3d::default(),
        Transform::from_xyz(0.0, 7., 14.0).looking_at(Vec3::new(0., 1., 0.), Vec3::Y),
    ));
}

fn move_player(
    mut query: Query<(&mut Transform, &mut Player)>,
    time: Res<Time<Fixed>>,
    mut random: ResMut<Random32>,
) {
    for (mut transform, mut player) in &mut query {
        player.0.x += (random.next_range(-4.0..4.0) - transform.translation.x) * time.delta_secs();
        player.0.z += (random.next_range(-4.0..4.0) - transform.translation.z) * time.delta_secs();
        transform.translation += player.0 * time.delta_secs();
    }
}

fn run_script(
    world: &World,
    q_base: Query<(Entity, &MainScript, &MeshMaterial3d<StandardMaterial>)>,
    script_assets: Res<Assets<ScriptModuleAsset>>,
    asset_server: Res<AssetServer>,
    r_host: Res<ScriptHostState>,
    mut commands: Commands,
) {
    let Some(host) = r_host.0.lock().ok() else {
        return;
    };

    for (entity, ms, material) in q_base.iter() {
        if asset_server.is_loaded(&ms.0)
            && let Some(script) = script_assets.get(&ms.0)
        {
            let mut tracking = TrackingScope::new(Tick::default());
            let mut vm = VM::new(world, &host, &mut tracking);
            vm.set_owner(entity);
            let result = vm.run(&script.module, "main").unwrap();
            if let Ok(color) = vm.extract_value::<Color>(result) {
                commands.queue(SetMaterialBaseColor {
                    material_id: material.0.clone(),
                    base_color: color,
                });
            }
        }
    }
}

struct SetMaterialBaseColor {
    pub material_id: Handle<StandardMaterial>,
    pub base_color: Color,
}

impl Command for SetMaterialBaseColor {
    fn apply(self, world: &mut World) {
        let mut materials = world.resource_mut::<Assets<StandardMaterial>>();
        if let Some(material) = materials.get_mut(&self.material_id) {
            material.base_color = self.base_color;
        }
    }
}

pub fn close_on_esc(input: Res<ButtonInput<KeyCode>>, mut exit: MessageWriter<AppExit>) {
    if input.just_pressed(KeyCode::Escape) {
        exit.write(AppExit::Success);
    }
}

#[derive(Resource)]
struct Random32 {
    state: u32,
}

impl Random32 {
    // Generate a pseudo-random number
    fn next(&mut self) -> u32 {
        // Constants for 32-bit LCG (example values, you might want to choose different ones)
        let a: u32 = 1664525; // Multiplier
        let c: u32 = 1013904223; // Increment
        let m: u32 = 2u32.pow(31); // Modulus, often set to 2^31 for a 32-bit generator

        // Simple LCG formula: X_{n+1} = (aX_n + c) mod m
        self.state = (a.wrapping_mul(self.state).wrapping_add(c)) % m;
        self.state
    }

    fn next_range(&mut self, range: Range<f32>) -> f32 {
        (self.next() & 0xfffff) as f32 * (range.end - range.start) / (0xfffff as f32) + range.start
    }
}

impl Default for Random32 {
    fn default() -> Self {
        Self { state: 17 }
    }
}

fn color_srgb(ctx: &mut InvocationContext) -> Result<StackValue, VMError> {
    assert_eq!(ctx.num_arguments(), 3);
    let r = ctx.argument::<f32>(0)?;
    let g = ctx.argument::<f32>(1)?;
    let b = ctx.argument::<f32>(2)?;
    Ok(ctx.create_heap_ref(Color::srgb(r, g, b)))
}

fn color_srgba(ctx: &mut InvocationContext) -> Result<StackValue, VMError> {
    assert_eq!(ctx.num_arguments(), 4);
    let r = ctx.argument::<f32>(0)?;
    let g = ctx.argument::<f32>(1)?;
    let b = ctx.argument::<f32>(2)?;
    let a = ctx.argument::<f32>(3)?;
    Ok(ctx.create_heap_ref(Color::srgba(r, g, b, a)))
}

fn get_self(vm: &VM) -> Result<StackValue, VMError> {
    Ok(StackValue::Entity(vm.owner))
}

fn get_player(vm: &VM) -> Result<StackValue, VMError> {
    let player = vm
        .world
        .try_query_filtered::<Entity, With<Player>>()
        .unwrap()
        .single(vm.world)
        .expect("Player not found");
    Ok(StackValue::Entity(player))
}

fn entity_position(ctx: &mut InvocationContext, actor: StackValue) -> Result<StackValue, VMError> {
    let StackValue::Entity(entity) = actor else {
        panic!("Not an entity");
    };
    if let Some(transform) = ctx.component::<GlobalTransform>(entity) {
        Ok(ctx.create_heap_ref(transform.translation()))
    } else {
        Err(VMError::MissingComponent(TypeId::of::<GlobalTransform>()))
    }
}

/// string.len()
fn vec3_distance(ctx: &mut InvocationContext) -> Result<StackValue, VMError> {
    assert_eq!(ctx.num_arguments(), 2);
    let v0: Vec3 = ctx.argument(0)?;
    let v1: Vec3 = ctx.argument(1)?;
    Ok(StackValue::F32(v0.distance(v1)))
}
