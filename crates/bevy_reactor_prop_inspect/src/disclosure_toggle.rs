// use accesskit::Role;
use bevy::{
    app::{App, Plugin, PreUpdate},
    ecs::{
        component::Component,
        lifecycle::RemovedComponents,
        query::{Added, Changed, Has, Or, With},
        reflect::ReflectComponent,
        schedule::IntoScheduleConfigs as _,
        system::{Query, Res},
    },
    feathers::{cursor::EntityCursor, theme::UiTheme, tokens},
    input_focus::tab_navigation::TabIndex,
    math::Rot2,
    picking::{PickingSystems, hover::Hovered},
    reflect::{Reflect, prelude::ReflectDefault},
    scene2::{Scene, bsn},
    ui::{
        AlignItems, Checked, Display, InteractionDisabled, JustifyContent, Node, UiTransform, px,
    },
    ui_widgets::Checkbox,
};

use crate::property_inspector::icon;
// use bevy_a11y::AccessibilityNode;
// use bevy_app::{Plugin, PreUpdate};
// use bevy_core_widgets::{Callback, CoreCheckbox, ValueChange};
// use bevy_ecs::{
//     bundle::Bundle,
//     component::Component,
//     entity::Entity,
//     lifecycle::RemovedComponents,
//     query::{Added, Changed, Has, Or, With},
//     reflect::ReflectComponent,
//     schedule::IntoScheduleConfigs,
//     system::{Commands, In, Query, Res},
// };
// use bevy_input_focus::tab_navigation::TabIndex;
// use bevy_math::{curve::EaseFunction, Rot2};
// use bevy_picking::{hover::Hovered, PickingSystems};
// use bevy_reflect::{prelude::ReflectDefault, Reflect};
// use bevy_ui::{
//     BackgroundColor, BorderColor, Checked, InteractionDisabled, Node, UiRect, Val,
// };

// use crate::{
//     constants::size,
//     cursor::EntityCursor,
//     palette,
//     theme::{ThemeBackgroundColor, ThemeBorderColor, UiTheme},
//     tokens,
//     transition::{AnimatedTransition, BackgroundColorTransition, UiRotationTransition},
// };

/// Marker for the disclosure toggle widget
#[derive(Component, Default, Clone, Reflect)]
#[reflect(Component, Clone, Default)]
struct DisclosureToggleStyle;

/// A toggle button which shows a chevron that points either right or down.
pub fn disclosure_toggle() -> impl Scene {
    bsn!(
        Node {
            // width: size::DISCLOSURE_SIZE,
            // height: size::DISCLOSURE_SIZE,
            width: px(12),
            height: px(12),
            display: Display::Flex,
            align_items: AlignItems::Center,
            justify_content: JustifyContent::Center,
        }
        Checkbox
        DisclosureToggleStyle
        // AnimatedTransition::<UiRotationTransition>::new(
        //     Rot2::turn_fraction(-0.125),
        //     Rot2::turn_fraction(0.125),
        // )
        // .with_duration(0.25)
        // .with_ease(EaseFunction::QuadraticInOut),
        // AccessibilityNode(accesskit::Node::new(Role::Switch)),
        EntityCursor::System(bevy::window::SystemCursorIcon::Pointer)
        TabIndex(0)
        [
            :icon("embedded://bevy_reactor_prop_inspect/assets/icons/chevron-right.png")
        ]
    )
}

#[allow(clippy::type_complexity)]
fn update_toggle_styles(
    mut q_switches: Query<
        (
            Has<InteractionDisabled>,
            Has<Checked>,
            &mut UiTransform,
            // &mut AnimatedTransition<UiRotationTransition>,
        ),
        (
            With<DisclosureToggleStyle>,
            Or<(Changed<Hovered>, Added<Checked>, Added<InteractionDisabled>)>,
        ),
    >,
    theme: Res<UiTheme>,
) {
    for (disabled, checked, mut transform) in q_switches.iter_mut() {
        set_toggle_colors(disabled, checked, transform.as_mut(), &theme);
    }
}

#[allow(clippy::type_complexity)]
fn update_toggle_styles_remove(
    mut q_switches: Query<
        (
            Has<InteractionDisabled>,
            Has<Checked>,
            &mut UiTransform,
            // &mut AnimatedTransition<UiRotationTransition>,
        ),
        With<DisclosureToggleStyle>,
    >,
    mut removed_disabled: RemovedComponents<InteractionDisabled>,
    mut removed_checked: RemovedComponents<Checked>,
    theme: Res<UiTheme>,
) {
    removed_disabled
        .read()
        .chain(removed_checked.read())
        .for_each(|ent| {
            if let Ok((disabled, checked, mut transform)) = q_switches.get_mut(ent) {
                set_toggle_colors(disabled, checked, transform.as_mut(), &theme);
            }
        });
}

fn set_toggle_colors(
    disabled: bool,
    checked: bool,
    // transform: &mut AnimatedTransition<UiRotationTransition>,
    transform: &mut UiTransform,
    _theme: &Res<'_, UiTheme>,
) {
    let _slide_token = match disabled {
        true => tokens::SWITCH_SLIDE_DISABLED,
        false => tokens::SWITCH_SLIDE,
    };

    match checked {
        true => {
            transform.rotation = Rot2::turn_fraction(0.25);
            // transform.start();
        }
        false => {
            transform.rotation = Rot2::turn_fraction(0.0);
            // transform.reverse();
        }
    };

    // Change outline border
    // if outline_border.0 != outline_border_token {
    //     commands
    //         .entity(switch_ent)
    //         .insert(ThemeBorderColor(outline_border_token));
    // }
}

/// Plugin which registers the systems for updating the toggle switch styles.
pub struct DisclosureTogglePlugin;

impl Plugin for DisclosureTogglePlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(
            PreUpdate,
            (update_toggle_styles, update_toggle_styles_remove).in_set(PickingSystems::Last),
        );
    }
}
