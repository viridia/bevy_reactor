use std::sync::Arc;

use crate::types::{bool_editor, f32_editor};
use crate::{
    Inspectable,
    InspectorFactory,
    // inspectors::{
    //     bool::BooleanFieldInspector, color::SrgbaInspector, r#enum::EnumInspector,
    //     f32::F32FieldInspector, fallback::FallbackInspector, list::ListInspector,
    //     r#struct::NestedStruct, tuple_struct::NestedTupleStruct, vec3::Vec3FieldInspector,
    // },
    // templates::{field_label::FieldLabel, field_readonly_value::FieldReadonlyValue},
};
use bevy::ecs::entity::Entity;
use bevy::ecs::hierarchy::Children;
use bevy::ecs::world::World;
use bevy::reflect::ReflectRef;
use bevy::scene2::{SpawnRelatedScenes, bsn_list};
use bevy::ui::widget::Text;

#[derive(Default)]
pub struct DefaultInspectorFactory;

impl InspectorFactory for DefaultInspectorFactory {
    fn spawn_inspector(&self, world: &mut World, parent: Entity, field: Arc<Inspectable>) -> bool {
        let Some(reflect) = field.reflect(world) else {
            return false;
        };
        match reflect.reflect_ref() {
            ReflectRef::Struct(_) => {
                world
                    .entity_mut(parent)
                    .spawn_related_scenes::<Children>(bsn_list!(Text::new("Struct")));
                true
            }
            ReflectRef::TupleStruct(_tuple_struct) => {
                world
                    .entity_mut(parent)
                    .spawn_related_scenes::<Children>(bsn_list!(Text::new("TupleStruct")));
                true
            }
            ReflectRef::Tuple(_tuple) => {
                world
                    .entity_mut(parent)
                    .spawn_related_scenes::<Children>(bsn_list!(Text::new("Tuple")));
                true
            }
            ReflectRef::List(_list) => {
                world
                    .entity_mut(parent)
                    .spawn_related_scenes::<Children>(bsn_list!(Text::new("List")));
                true
            }
            ReflectRef::Array(_array) => {
                world
                    .entity_mut(parent)
                    .spawn_related_scenes::<Children>(bsn_list!(Text::new("Array")));
                true
            }
            ReflectRef::Map(_map) => {
                world
                    .entity_mut(parent)
                    .spawn_related_scenes::<Children>(bsn_list!(Text::new("Map")));
                true
            }
            ReflectRef::Set(_set) => {
                world
                    .entity_mut(parent)
                    .spawn_related_scenes::<Children>(bsn_list!(Text::new("Set")));
                true
            }
            ReflectRef::Enum(_) => {
                world
                    .entity_mut(parent)
                    .spawn_related_scenes::<Children>(bsn_list!(Text::new("Enum")));
                true
            }
            ReflectRef::Opaque(partial_reflect) => {
                match partial_reflect.reflect_type_path() {
                    "bool" => {
                        world
                            .entity_mut(parent)
                            .spawn_related_scenes::<Children>(bool_editor(field));
                    }
                    "f32" => {
                        world
                            .entity_mut(parent)
                            .spawn_related_scenes::<Children>(f32_editor(field));
                    }
                    _ => {
                        world
                            .entity_mut(parent)
                            .spawn_related_scenes::<Children>(bsn_list!(Text::new("Opaque")));
                    }
                }
                true
            }
        }
        // match reflect.reflect_ref() {
        //     ReflectRef::Struct(s) => match s.reflect_type_path() {
        //         "bevy_color::srgba::Srgba" => Some(SrgbaInspector(field.clone()).into_view()),
        //         "glam::Vec3" => Some(Vec3FieldInspector(field.clone()).into_view()),
        //         _ => Some(NestedStruct(field.clone()).into_view()),
        //     },
        //     ReflectRef::TupleStruct(_) => Some(NestedTupleStruct(field.clone()).into_view()),
        //     ReflectRef::Tuple(_) => Some(
        //         Fragment::new((
        //             FieldLabel {
        //                 field: field.clone(),
        //             },
        //             FieldReadonlyValue::new().children("Tuple:TODO"),
        //         ))
        //         .into_view(),
        //     ),
        //     ReflectRef::List(_) => Some(ListInspector(field.clone()).into_view()),
        //     ReflectRef::Array(_) => Some(
        //         Fragment::new((
        //             FieldLabel {
        //                 field: field.clone(),
        //             },
        //             FieldReadonlyValue::new().children("Array:TODO"),
        //         ))
        //         .into_view(),
        //     ),
        //     ReflectRef::Map(_) => Some(
        //         Fragment::new((
        //             FieldLabel {
        //                 field: field.clone(),
        //             },
        //             FieldReadonlyValue::new().children("Map:TODO"),
        //         ))
        //         .into_view(),
        //     ),
        //     ReflectRef::Enum(_) => Some(EnumInspector(field.clone()).into_view()),
        //     ReflectRef::Value(v) => match v.reflect_type_path() {
        //         "bool" => Some(BooleanFieldInspector(field.clone()).into_view()),
        //         "f32" => Some(F32FieldInspector(field.clone()).into_view()),
        //         _ => Some(FallbackInspector(field.clone()).into_view()),
        //     },
        // }
    }
}
