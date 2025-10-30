use std::sync::Arc;

use bevy::{
    ecs::{observer::On, world::DeferredWorld},
    feathers::{controls::checkbox, theme::ThemedText},
    reflect::Reflect,
    scene2::{SceneList, bsn_list, on},
    ui::{Checked, widget::Text},
    ui_widgets::ValueChange,
};
use bevy_reactor::{Cx, effect};

use crate::Inspectable;

pub fn bool_editor(field: Arc<Inspectable>) -> impl SceneList {
    let field_copy = field.clone();
    let field_copy2 = field.clone();
    bsn_list![
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
        ]
    ]
}
