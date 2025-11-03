use std::sync::Arc;

use bevy::{
    feathers::{
        constants::fonts,
        font_styles::InheritableFont,
        theme::{ThemeFontColor, ThemedText},
        tokens,
    },
    math::Vec3,
    scene2::{SceneList, bsn_list},
    ui::{AlignItems, Display, FlexDirection, JustifyContent, Node, px, widget::Text},
};
use bevy_reactor::{Cx, effect};

use crate::{
    Inspectable,
    property_inspector::{field_group, field_label},
};

pub fn vec3_field(field: Arc<Inspectable>) -> impl SceneList {
    let field_copy = field.clone();
    let field_copy2 = field.clone();
    let field_copy3 = field.clone();
    bsn_list![
        :field_group
        [
            :field_label(field)
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
                font_size: 16.0,
            }
            // ThemeFontColor(tokens::TEXT_DIM)
            ThemeFontColor(tokens::CHECKBOX_TEXT)
            [
                Text("x:") ThemedText,
                Text("")
                ThemedText
                effect::memo_effect(move |cx: &Cx| {
                    let reflect = field_copy.reflect_tracked(cx).unwrap();
                    if let Some(value) = reflect.try_downcast_ref::<Vec3>() {
                        return value.x;
                    }
                    0.0
                }, |entity, x| {
                    if let Some(mut text) = entity.get_mut::<Text>() {
                        text.0 = format!("{x}");
                    }
                }),

                Text("y:") ThemedText,
                Text("")
                ThemedText
                effect::memo_effect(move |cx: &Cx| {
                    let reflect = field_copy2.reflect_tracked(cx).unwrap();
                    if let Some(value) = reflect.try_downcast_ref::<Vec3>() {
                        return value.y;
                    }
                    0.0
                }, |entity, y| {
                    if let Some(mut text) = entity.get_mut::<Text>() {
                        text.0 = format!("{y}");
                    }
                }),

                Text("z:") ThemedText,
                Text("")
                ThemedText
                effect::memo_effect(move |cx: &Cx| {
                    let reflect = field_copy3.reflect_tracked(cx).unwrap();
                    if let Some(value) = reflect.try_downcast_ref::<Vec3>() {
                        return value.z;
                    }
                    0.0
                }, |entity, z| {
                    if let Some(mut text) = entity.get_mut::<Text>() {
                        text.0 = format!("{z}");
                    }
                }),
            ]
        ]
    ]
}
