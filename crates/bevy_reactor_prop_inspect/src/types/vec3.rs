use std::sync::Arc;

use bevy::{
    ecs::{hierarchy::Children, observer::On, world::DeferredWorld},
    feathers::{
        constants::fonts,
        controls::{FeathersNumberInput, NumberInputPrecision, NumberInputValue},
        font_styles::InheritableFont,
        palette,
        theme::ThemeTextColor,
        tokens,
    },
    math::Vec3,
    scene::{SceneList, bsn_list, on},
    text::FontSize,
    ui::{AlignItems, BorderColor, Display, FlexDirection, JustifyContent, Node, px},
    ui_widgets::ValueChange,
};
use bevy_reactor::{Cx, effect};

use crate::{
    Inspectable,
    property_inspector::{field_group, field_label},
};

pub fn vec3_field(field: Arc<Inspectable>) -> impl SceneList {
    let field_copy = field.clone();
    bsn_list![
        field_group()
        Children [
            field_label(field)
            ,
            Node {
                display: Display::Flex,
                flex_direction: FlexDirection::Row,
                align_items: AlignItems::Center,
                justify_content: JustifyContent::Start,
                column_gap: px(6),
            }
            InheritableFont {
                font: fonts::REGULAR,
                font_size: FontSize::Px(16.0),
            }
            // ThemeTextColor(tokens::TEXT_DIM)
            ThemeTextColor(tokens::CHECKBOX_TEXT)
            Children [
                (
                    @FeathersNumberInput {
                        @sigil_color: tokens::TEXT_INPUT_X_AXIS,
                        @label_text: "X",
                    }
                    NumberInputPrecision(2)
                    Node {
                        flex_grow: 1.0,
                    }
                    BorderColor::all(palette::X_AXIS)
                    on({
                        let field = field_copy.clone();
                        move |value_change: On<ValueChange<f32>>, mut world: DeferredWorld| {
                            field.update_value(&mut world, &|reflect| {
                                if let Some(value) = reflect.try_downcast_mut::<Vec3>() {
                                    value.x = value_change.value;
                                }
                            });
                        }
                    })
                    effect::memo_effect({
                        let field = field_copy.clone();
                        move |cx: &Cx| {
                            let reflect = field.reflect_tracked(cx).unwrap();
                            if let Some(value) = reflect.try_downcast_ref::<Vec3>() {
                                return value.x;
                            }
                            0.0
                        }
                    }, |entity, x| {
                        entity.insert(NumberInputValue::F32(*x));
                    })
                ),

                (
                    @FeathersNumberInput {
                        @sigil_color: tokens::TEXT_INPUT_Y_AXIS,
                        @label_text: "X",
                    }
                    NumberInputPrecision(2)
                    Node {
                        flex_grow: 1.0,
                    }
                    BorderColor::all(palette::Y_AXIS)
                    on({
                        let field = field_copy.clone();
                        move |value_change: On<ValueChange<f32>>, mut world: DeferredWorld| {
                            field.update_value(&mut world, &|reflect| {
                                if let Some(value) = reflect.try_downcast_mut::<Vec3>() {
                                    value.y = value_change.value;
                                }
                            });
                        }
                    })
                    effect::memo_effect({
                        let field = field_copy.clone();
                        move |cx: &Cx| {
                            let reflect = field.reflect_tracked(cx).unwrap();
                            if let Some(value) = reflect.try_downcast_ref::<Vec3>() {
                                return value.y;
                            }
                            0.0
                        }
                    }, |entity, y| {
                        entity.insert(NumberInputValue::F32(*y));
                    })
                ),

                (
                    @FeathersNumberInput {
                        @sigil_color: tokens::TEXT_INPUT_Z_AXIS,
                        @label_text: "Z",
                    }
                    NumberInputPrecision(2)
                    Node {
                        flex_grow: 1.0,
                    }
                    BorderColor::all(palette::Z_AXIS)
                    on({
                        let field = field_copy.clone();
                        move |value_change: On<ValueChange<f32>>, mut world: DeferredWorld| {
                            field.update_value(&mut world, &|reflect| {
                                if let Some(value) = reflect.try_downcast_mut::<Vec3>() {
                                    value.z = value_change.value;
                                }
                            });
                        }
                    })
                    effect::memo_effect({
                        let field = field_copy.clone();
                        move |cx: &Cx| {
                            let reflect = field.reflect_tracked(cx).unwrap();
                            if let Some(value) = reflect.try_downcast_ref::<Vec3>() {
                                return value.z;
                            }
                            0.0
                        }
                    }, |entity, z| {
                        entity.insert(NumberInputValue::F32(*z));
                    })
                ),
            ]
        ]
    ]
}
