use std::sync::Arc;

use bevy::{
    ecs::{hierarchy::Children, observer::On, world::DeferredWorld},
    feathers::controls::{
        FeathersNumberInput, HardLimit, NumberInputPrecision, NumberInputRange, NumberInputStep,
        NumberInputValue, SoftLimit,
    },
    reflect::Reflect,
    scene::{SceneList, bsn_list, on},
    ui::Node,
    ui_widgets::ValueChange,
};
use bevy_reactor::{Cx, effect};

use crate::{
    Inspectable, Precision, Step, ValueRange,
    property_inspector::{field_group, field_label},
};

pub fn f32_field(field: Arc<Inspectable>) -> impl SceneList {
    let range = if let Some(attrs) = field.attributes
        && let Some(range) = attrs.get::<ValueRange<f32>>()
    {
        Some(range.0.clone())
    } else {
        None
    };

    let precision = if let Some(attrs) = field.attributes
        && let Some(precision) = attrs.get::<Precision>()
    {
        Some(precision.0)
    } else {
        None
    };

    let step = if let Some(attrs) = field.attributes
        && let Some(step) = attrs.get::<Step<f32>>()
    {
        Some(step.0 as f64)
    } else {
        None
    };

    let field_copy = field.clone();
    bsn_list![
        field_group()
        Children [
            field_label(field)
            ,
            @FeathersNumberInput
            Node {
                flex_grow: 1.0,
            }
            on({
                let field = field_copy.clone();
                move |value_change: On<ValueChange<f32>>, mut world: DeferredWorld| {
                    field.set_value(&mut world, value_change.value.as_reflect());
                }
            })
            effect::memo_effect({
                let field = field_copy.clone();
                move |cx: &Cx| {
                    let reflect = field.reflect_tracked(cx).unwrap();
                    if let Some(value) = reflect.try_downcast_ref::<f32>() {
                        return *value;
                    }
                    0.0
                }
            }, |entity, value| {
                entity.insert(NumberInputValue::F32(*value));
            })
            effect::insert_computed_when(
                { let r = range.clone(); move |_: &Cx| r.clone() },
                |range| HardLimit(NumberInputRange::F32(range.clone())))
            effect::insert_computed_when(
                { let r = range.clone(); move |_: &Cx| r.clone() },
                |range| SoftLimit(NumberInputRange::F32(range.clone())))
            effect::insert_computed_when(move |_: &Cx| precision, NumberInputPrecision)
            effect::insert_computed_when(move |_: &Cx| step, NumberInputStep)
            on({
                let field = field_copy.clone();
                move |value_change: On<ValueChange<f32>>, mut world: DeferredWorld| {
                    field.set_value(&mut world, value_change.value.as_reflect());
                }
            })
        ]
    ]
}
