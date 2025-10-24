//! Example which uses states and a switch view.

use bevy::{
    color::palettes::css,
    prelude::*,
    scene2::{CommandsSpawnScene, bsn, bsn_list},
    ui,
};
use bevy_reactor::{Cx, ReactorPlugin, for_each};

fn main() {
    App::new()
        .init_resource::<List>()
        .init_resource::<Random32>()
        .add_plugins((
            DefaultPlugins.set(ImagePlugin::default_nearest()),
            ReactorPlugin,
        ))
        .add_systems(Startup, setup_view_root)
        .add_systems(Update, (close_on_esc, handle_key_input))
        .run();
}

const SUITS: &[&str] = &["hearts", "spades", "clubs", "diamonds"];

#[derive(Resource, Default)]
pub struct List {
    pub items: Vec<String>,
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
            flex_direction: ui::FlexDirection::Column,
            border: ui::UiRect::all(ui::Val::Px(3.)),
        }
        BorderColor::all(css::ALICE_BLUE)
        [
            for_each(
                |cx: &Cx| {
                    cx.resource::<List>().items.clone()
                },
                move |suit: &String, _index| {
                    let suit = suit.clone();
                    bsn_list![
                        (
                            Node {
                                border: ui::UiRect::all(ui::Val::Px(3.)),
                            }
                            BorderColor::all(css::GREEN)
                            [
                                Text::new(suit.clone()),
                            ]
                        )
                    ]
                },
                || bsn_list![Text::new("No items")]
            )
        ]
    ));
}

fn handle_key_input(
    key: Res<ButtonInput<KeyCode>>,
    mut list: ResMut<List>,
    mut random: ResMut<Random32>,
) {
    if key.just_pressed(KeyCode::Space) {
        println!("-- Space pressed --");
        let i = (random.next() as usize) % SUITS.len();
        list.items.push(SUITS[i].to_string());
        while list.items.len() > 10 {
            list.items.remove(0);
        }
    } else if key.just_pressed(KeyCode::Minus) {
        println!("-- Minus pressed --");
        list.items.pop();
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
}

impl Default for Random32 {
    fn default() -> Self {
        Self { state: 17 }
    }
}
