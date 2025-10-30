//! Example which uses states and a switch view.

use std::sync::Arc;

use bevy::{
    feathers::{
        FeathersPlugins,
        dark_theme::create_dark_theme,
        theme::{ThemeBackgroundColor, UiTheme},
        tokens,
    },
    prelude::*,
    scene2::{CommandsSpawnScene, bsn},
    ui,
};
// use bevy_inspector_egui::quick::WorldInspectorPlugin;
use bevy_reactor::ReactorPlugin;
use bevy_reactor_prop_inspect::{
    InspectableResource, Precision, PropertyInspectorPlugin, ValueRange, property_inspector,
};

fn main() {
    App::new()
        .add_plugins((
            DefaultPlugins.set(ImagePlugin::default_nearest()),
            FeathersPlugins,
            ReactorPlugin,
            PropertyInspectorPlugin,
        ))
        // .add_plugins(WorldInspectorPlugin::new())
        .insert_resource(UiTheme(create_dark_theme()))
        .insert_resource(TestStruct {
            unlit: Some(true),
            ..default()
        })
        .insert_resource(TestStruct2 {
            nested: TestStruct::default(),
            ..default()
        })
        .insert_resource(TestStruct3(true))
        .add_systems(Startup, setup_view_root)
        .add_systems(Update, close_on_esc)
        .run();
}

#[derive(Debug, Reflect, Clone, Default)]
pub enum TestEnum {
    #[default]
    Unit,
    Float(f32),
    Color(Srgba),
    Struct {
        position: Vec3,
        color: Srgba,
    },
}

#[derive(Resource, Debug, Reflect, Clone, Default)]
pub struct TestStruct {
    pub selected: bool,

    #[reflect(@ValueRange::<f32>(0.0..1.0))]
    pub scale: f32,

    pub color: Srgba,
    pub position: Vec3,
    pub unlit: Option<bool>,

    #[reflect(@ValueRange::<f32>(0.0..10.0))]
    pub roughness: Option<f32>,

    #[reflect(@Precision(2))]
    pub metalness: Option<f32>,

    #[reflect(@ValueRange::<f32>(0.0..1000.0))]
    pub factors: Vec<f32>,
}

#[derive(Resource, Debug, Reflect, Clone, Default)]
pub struct TestStruct2 {
    pub nested: TestStruct,
    pub choice: TestEnum,
}

#[derive(Resource, Debug, Reflect, Clone, Default)]
pub struct TestStruct3(pub bool);

// pub struct ResourcePropertyInspector<T: Resource> {
//     marker: std::marker::PhantomData<T>,
// }

// impl<T: Resource> ResourcePropertyInspector<T> {
//     pub fn new() -> Self {
//         Self {
//             marker: std::marker::PhantomData,
//         }
//     }
// }

// impl<T: Resource + Reflect> ViewTemplate for ResourcePropertyInspector<T> {
//     fn create(&self, _cx: &mut Cx) -> impl IntoView {
//         Inspector::new(Arc::<InspectableResource<T>>::default())
//     }
// }

fn setup_view_root(mut commands: Commands) {
    commands.spawn((Camera::default(), Camera2d));
    let root = Arc::new(InspectableResource::<TestStruct>::default());

    commands.spawn_scene(bsn!(
        Node {
            left: px(0),
            top: px(0),
            bottom: px(0),
            width: px(300),
            position_type: ui::PositionType::Absolute,
            display: ui::Display::Flex,
            flex_direction: ui::FlexDirection::Column,
            padding: {px(6).all()},
            border: ui::UiRect::all(ui::Val::Px(3.)),
        }
        ThemeBackgroundColor(tokens::WINDOW_BG)
        [
            (
                :property_inspector(root)
            )
        ]
    ));
}

pub fn close_on_esc(input: Res<ButtonInput<KeyCode>>, mut exit: MessageWriter<AppExit>) {
    if input.just_pressed(KeyCode::Escape) {
        exit.write(AppExit::Success);
    }
}
