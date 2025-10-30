use std::sync::Arc;

use bevy::{
    ecs::{observer::On, world::DeferredWorld},
    feathers::{
        controls::{SliderProps, slider},
        theme::ThemedText,
    },
    reflect::Reflect,
    scene2::{SceneList, bsn_list, on},
    ui::widget::Text,
    ui_widgets::{SliderValue, ValueChange},
};
use bevy_reactor::{Cx, effect};

use crate::{Inspectable, Precision, Step, ValueRange};

pub fn f32_editor(field: Arc<Inspectable>) -> impl SceneList {
    if let Some(attrs) = field.attributes {
        if let Some(range) = attrs.get::<ValueRange<f32>>() {
            // slider_params.range = Some(range.0.clone());
            // slider_params.precision =
            //     (2. - (range.0.end - range.0.start).log10().ceil()).max(0.) as usize;
        }
        //     if let Some(precision) = attrs.get::<Precision>() {
        //         slider_params.precision = precision.0;
        //     }
        //     if let Some(step) = attrs.get::<Step<f32>>() {
        //         slider_params.step = step.0;
        //     } else {
        //         slider_params.step = 10.0f32.powi(-(slider_params.precision as i32));
        //     }
    }

    let field_copy = field.clone();
    let field_copy2 = field.clone();
    bsn_list![
        // [
        //     Text({field.name.to_owned()}) ThemedText
        // ],
        :slider(SliderProps {
            min: 0.,
            max: 100.,
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
        on(move |value_change: On<ValueChange<f32>>, mut world: DeferredWorld| {
            field_copy2.set_value(&mut world, value_change.value.as_reflect());
        })
    ]
}
