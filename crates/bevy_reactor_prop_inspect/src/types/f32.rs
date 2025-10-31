use std::sync::Arc;

use bevy::{
    ecs::{observer::On, world::DeferredWorld},
    feathers::controls::{SliderProps, slider},
    reflect::Reflect,
    scene2::{SceneList, bsn_list, on},
    ui::widget::Text,
    ui_widgets::{SliderPrecision, SliderStep, SliderValue, ValueChange},
};
use bevy_reactor::{
    Cx,
    effect::{self, insert_computed_when},
};

use crate::{Inspectable, Precision, Step, ValueRange, property_inspector::field_label};

pub fn f32_range_field(field: Arc<Inspectable>, range: &ValueRange<f32>) -> impl SceneList {
    let field_copy = field.clone();
    let field_copy2 = field.clone();
    let field_copy3 = field.clone();
    let field_copy4 = field.clone();
    bsn_list![
        :field_label(field)
        ,
        :slider(SliderProps {
            min: range.0.start,
            max: range.0.end,
            value: 0.,
        })
        effect::memo_effect(move |cx: &Cx| {
            let reflect = field_copy.reflect_tracked(cx).unwrap();
            if let Some(value) = reflect.try_downcast_ref::<f32>() {
                return *value;
            }
            0.0
        }, |entity, value| {
            entity.insert(SliderValue(*value));
        })
        insert_computed_when(move |_cx: &Cx| {
            if let Some(attrs) = field_copy3.attributes && let Some(precision) = attrs.get::<Precision>() {
                Some(precision.0)
            } else {
                None
            }
        }, SliderPrecision)
        insert_computed_when(move |_cx: &Cx| {
            if let Some(attrs) = field_copy4.attributes && let Some(step) = attrs.get::<Step<f32>>() {
                Some(step.0)
            } else {
                None
            }
        }, SliderStep)
        on(move |value_change: On<ValueChange<f32>>, mut world: DeferredWorld| {
            field_copy2.set_value(&mut world, value_change.value.as_reflect());
        })
    ]
}

pub fn f32_field(field: Arc<Inspectable>) -> impl SceneList {
    let field_copy = field.clone();
    bsn_list![
        :field_label(field)
        ,
        :slider(SliderProps {
            min: 0.,
            max: 100.,
            value: 0.,
        })
        Text("")
        effect::memo_effect(move |cx: &Cx| {
            let reflect = field_copy.reflect_tracked(cx).unwrap();
            if let Some(value) = reflect.try_downcast_ref::<f32>() {
                return *value;
            }
            0.0
        }, |entity, value| {
            if let Some(mut text) = entity.get_mut::<Text>() {
                text.0 = format!("{value}");
            }
        })
    ]
}
