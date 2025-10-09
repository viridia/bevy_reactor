//! Example which uses states and a switch view.

use bevy::{
    color::palettes::css,
    input_focus::tab_navigation::TabIndex,
    picking::hover::Hovered,
    prelude::*,
    scene2::{CommandsSpawnScene, Scene, bsn},
    ui::{self, InteractionDisabled, Pressed},
    ui_widgets::UiWidgetsPlugins,
};
use bevy_reactor::{Cx, ReactorPlugin, effect::insert_dyn};

fn main() {
    App::new()
        .add_plugins((
            DefaultPlugins.set(ImagePlugin::default_nearest()),
            UiWidgetsPlugins,
            ReactorPlugin,
        ))
        .add_systems(Startup, setup_view_root)
        .add_systems(Update, close_on_esc)
        .run();
}

fn setup_view_root(mut commands: Commands) {
    commands.spawn((Camera::default(), Camera2d));

    commands.spawn_scene(bsn!(
        Node {
            left: ui::Val::Px(0.),
            top: ui::Val::Px(0.),
            right: ui::Val::Px(0.),
            bottom: ui::Val::Px(0.),
            position_type: ui::PositionType::Absolute,
            display: ui::Display::Flex,
            flex_direction: ui::FlexDirection::Row,
            align_items: ui::AlignItems::Center,
            justify_content: ui::JustifyContent::Center,
            border: ui::UiRect::all(ui::Val::Px(3.)),
        }
        BorderColor::all(css::ALICE_BLUE)
        // insert_dyn(
        //     |cx: &Cx| *cx.resource::<State<GameState>>().get(),
        //     |state| BackgroundColor(match state {
        //         GameState::Play => css::DARK_GREEN.into(),
        //         GameState::Pause => css::DARK_GRAY.into(),
        //         GameState::Intro => css::BLUE.into(),
        //     })
        // )
        [
            (:button()
            [
                Text("Foo")

            ])
            // switch(|cx: &Cx| *cx.resource::<State<GameState>>().get(), |cases| {
            //     cases.case(GameState::Play, bsn_list!(Text("Playing")))
            //         .fallback(bsn_list!(Text("Not Playing")));
            // })
        ]
    ));
}

const NORMAL_BUTTON: Color = Color::srgb(0.15, 0.15, 0.15);
const HOVERED_BUTTON: Color = Color::srgb(0.25, 0.25, 0.25);
const PRESSED_BUTTON: Color = Color::srgb(0.35, 0.75, 0.35);

fn button() -> impl Scene {
    bsn! {
        Node {
            width: Val::Px(150.0),
            height: Val::Px(65.0),
            border: UiRect::all(Val::Px(5.0)),
            justify_content: JustifyContent::Center,
            align_items: AlignItems::Center,
        }
        Button
        Hovered::default()
        TabIndex(0)
        BorderColor::all(Color::BLACK)
        BorderRadius::MAX
        insert_dyn(|cx: &Cx| {
            let entity = cx.owner();
            match (
                entity.contains::<InteractionDisabled>(),
                entity.contains::<Pressed>(),
                entity.get::<Hovered>().map(|hovered| hovered.0).unwrap_or(false),
            ) {
                (true, _, _) => NORMAL_BUTTON,
                (false, true, _) => PRESSED_BUTTON,
                (false, false, true) => HOVERED_BUTTON,
                _ => NORMAL_BUTTON
            }
        }, BackgroundColor)
        [(
            Text::new("Button")
            TextColor(Color::srgb(0.9, 0.9, 0.9))
            TextShadow::default()
        )]
    }
}

pub fn close_on_esc(input: Res<ButtonInput<KeyCode>>, mut exit: MessageWriter<AppExit>) {
    if input.just_pressed(KeyCode::Escape) {
        exit.write(AppExit::Success);
    }
}
