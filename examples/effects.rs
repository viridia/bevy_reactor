//! Example which uses states and a switch view.

use bevy::{
    color::palettes::css,
    prelude::*,
    scene2::{CommandsSpawnScene, bsn},
    ui,
};
use bevy_reactor::{Cx, ReactorPlugin, effect::effect};

#[derive(States, Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum GameState {
    #[default]
    Play,
    Pause,
    Intro,
}

fn main() {
    App::new()
        .add_plugins((
            DefaultPlugins.set(ImagePlugin::default_nearest()),
            ReactorPlugin,
        ))
        .insert_state(GameState::Intro)
        .add_systems(Startup, setup_view_root)
        .add_systems(Update, (close_on_esc, handle_key_input))
        .run();
}

fn setup_view_root(mut commands: Commands) {
    commands.spawn((Camera::default(), Camera2d));

    commands.spawn_scene(bsn!(
        Node {
            left: ui::Val::Px(0.),
            top: ui::Val::Px(0.),
            right: ui::Val::Px(0.),
            position_type: ui::PositionType::Absolute,
            display: ui::Display::Flex,
            flex_direction: ui::FlexDirection::Row,
            border: ui::UiRect::all(ui::Val::Px(3.)),
        }
        BorderColor::all(css::ALICE_BLUE)
        effect(
            |cx: &Cx| *cx.resource::<State<GameState>>().get(),
            |entity, state| {
                entity.insert(BackgroundColor(match state {
                    GameState::Play => css::DARK_GREEN.into(),
                    GameState::Pause => css::DARK_GRAY.into(),
                    GameState::Intro => css::BLUE.into(),
                }));
            }
        )
        [
            Text("Game State: "),
            (
                Text("")
                effect(
                    |cx: &Cx| *cx.resource::<State<GameState>>().get(),
                    |entity, state| {
                        if let Some(mut text) = entity.get_mut::<Text>() {
                            text.0 = match state {
                                GameState::Play => "Play",
                                GameState::Pause => "Pause",
                                GameState::Intro => "Intro",
                            }.into()
                        }
                    })
            )
        ]
    ));
}

fn handle_key_input(
    state: Res<State<GameState>>,
    mut next_state: ResMut<NextState<GameState>>,
    key: Res<ButtonInput<KeyCode>>,
) {
    if key.just_pressed(KeyCode::Space) {
        match state.get() {
            GameState::Intro => next_state.set(GameState::Play),
            GameState::Play => next_state.set(GameState::Pause),
            GameState::Pause => next_state.set(GameState::Play),
        }
    }
}

pub fn close_on_esc(input: Res<ButtonInput<KeyCode>>, mut exit: EventWriter<AppExit>) {
    if input.just_pressed(KeyCode::Escape) {
        exit.write(AppExit::Success);
    }
}
