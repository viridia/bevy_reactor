use std::sync::Arc;

use bevy::{
    ecs::{observer::On, world::DeferredWorld},
    feathers::{controls::checkbox, theme::ThemedText},
    reflect::Reflect,
    scene2::{SceneList, bsn_list, on},
    ui::{AlignItems, Checked, Display, FlexDirection, JustifyContent, Node, widget::Text},
    ui_widgets::ValueChange,
};
use bevy_reactor::{Cx, effect, if_then};

use crate::{Inspectable, property_inspector::remove_button};

pub fn bool_field(field: Arc<Inspectable>) -> impl SceneList {
    let can_remove = field.can_remove;
    let field_copy = field.clone();
    let field_copy2 = field.clone();
    let field_copy3 = field.clone();
    bsn_list![
        Node {
            display: Display::Flex,
            flex_direction: FlexDirection::Row,
            align_items: AlignItems::Center,
            justify_content: JustifyContent::SpaceBetween,
        }
        [
            :checkbox()
            effect::memo_effect(move |cx: &Cx| {
                let reflect = field_copy.reflect_tracked(cx).unwrap();
                if let Some(value) = reflect.try_downcast_ref::<bool>() {
                    return *value;
                }
                false
            }, |entity, checked| {
                if *checked {
                    entity.insert(Checked);
                } else {
                    entity.remove::<Checked>();
                }
            })
            on(move |value_change: On<ValueChange<bool>>, mut world: DeferredWorld| {
                field_copy2.set_value(&mut world, value_change.value.as_reflect());
            })
            [
                Text({field.name.to_owned()}) ThemedText
            ],
            if_then(move |_: &Cx| can_remove, {
                let field = field_copy3.clone();
                move || bsn_list![:remove_button(field.clone())]
            }),
        ]
    ]
}
