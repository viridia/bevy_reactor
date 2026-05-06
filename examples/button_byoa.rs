//! Example which uses states and a switch view.

use bevy::{
    color::palettes::{self, css},
    feathers::cursor::EntityCursor,
    picking::hover::Hovered,
    prelude::*,
    scene::bsn,
    text::FontSourceTemplate,
    ui::{self, widget::ImageNodeTemplate},
    ui_widgets::Button,
    window::SystemCursorIcon,
};
use bevy_keratin::{
    StatePattern, StateSource, StyleRule, VisualStates, WidgetState, update_widget_styles,
};

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_systems(Startup, setup_view_root)
        .add_systems(Update, (update_widget_styles, close_on_esc))
        .run();
}

fn setup_view_root(mut commands: Commands) {
    commands.spawn((Camera::default(), Camera2d));

    commands.spawn_scene(bsn!(
        Node {
            left: px(0),
            top: px(0),
            right: px(0),
            bottom: px(0),
            position_type: ui::PositionType::Absolute,
            display: ui::Display::Flex,
            flex_direction: ui::FlexDirection::Row,
            align_items: ui::AlignItems::Center,
            justify_content: ui::JustifyContent::Center,
            border: ui::UiRect::all(px(3)),
            column_gap: px(8),
        }
        BorderColor::all(css::ALICE_BLUE)
        Children [
            (
                :button()
                Children [
                    :button_caption("Retreat")
                ]
            ),
            (
                :button()
                Children [
                    :button_caption("Attack")
                ]
            )
        ]
    ));
}

fn close_on_esc(input: Res<ButtonInput<KeyCode>>, mut exit: MessageWriter<AppExit>) {
    if input.just_pressed(KeyCode::Escape) {
        exit.write(AppExit::Success);
    }
}

fn button() -> impl Scene {
    let slicer = TextureSlicer {
        border: BorderRect::all(16.0),
        center_scale_mode: SliceScaleMode::Stretch,
        sides_scale_mode: SliceScaleMode::Stretch,
        max_corner_scale: 1.0,
    };
    bsn! {
        Node {
            display: Display::Flex,
            flex_direction: FlexDirection::Row,
            min_height: px(32),
            align_items: AlignItems::Center,
            justify_content: JustifyContent::Center,
            padding: UiRect::axes(px(20), px(0)),
        }
        Button
        Hovered
        EntityCursor::System(SystemCursorIcon::Pointer)
        StateSource::SelfState
        VisualStates [
            StyleRule {
                pattern: StatePattern {
                    mask: WidgetState::DISABLED,
                    required: WidgetState::DISABLED,
                },
                background: {ImageNodeTemplate {
                    color: Color::WHITE,
                    image: "button_normal.png".into(),
                    image_mode: NodeImageMode::Sliced(slicer.clone()),
                    visual_box: VisualBox::BorderBox,
                    ..default()
                }},
            },
            StyleRule {
                pattern: StatePattern {
                    mask: WidgetState::PRESSED,
                    required: WidgetState::PRESSED,
                },
                background: {ImageNodeTemplate {
                    color: palettes::css::RED.into(),
                    image: "button_normal.png".into(),
                    image_mode: NodeImageMode::Sliced(slicer.clone()),
                    visual_box: VisualBox::BorderBox,
                    ..default()
                }},
            },
            StyleRule {
                pattern: StatePattern {
                    mask: WidgetState::HOVERED,
                    required: WidgetState::HOVERED,
                },
                background: {ImageNodeTemplate {
                    color: palettes::css::ALICE_BLUE.into(),
                    image: "button_normal.png".into(),
                    image_mode: NodeImageMode::Sliced(slicer.clone()),
                    visual_box: VisualBox::BorderBox,
                    ..default()
                }},
            },
            StyleRule {
                pattern: StatePattern {
                    mask: WidgetState::empty(),
                    required: WidgetState::empty(),
                },
                background: {ImageNodeTemplate {
                    color: palettes::css::AQUAMARINE.into(),
                    image: "button_normal.png".into(),
                    image_mode: NodeImageMode::Sliced(slicer.clone()),
                    visual_box: VisualBox::BorderBox,
                    ..default()
                }},
            },
        ]
    }
}

fn button_caption(text: &'static str) -> impl Scene {
    bsn! {
        Text(text)
        TextColor(palettes::css::BLACK)
        TextFont {
            font: FontSourceTemplate::SansSerif,
            font_size: FontSize::Px(20.0)
            weight: FontWeight::BOLD,
        }
    }
}
