use std::sync::Arc;

use crate::property_inspector::{field_group, field_label};
use crate::types::{bool_field, f32_field, f32_range_field, list_field, srgba_field, vec3_field};
use crate::{Inspectable, InspectorFactory, ValueRange};
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
            ReflectRef::Struct(s) => match s.reflect_type_path() {
                "bevy_color::srgba::Srgba" => {
                    world
                        .entity_mut(parent)
                        .spawn_related_scenes::<Children>(srgba_field(field));
                    true
                }
                "glam::Vec3" => {
                    world
                        .entity_mut(parent)
                        .spawn_related_scenes::<Children>(vec3_field(field));
                    true
                }
                _ => {
                    world
                        .entity_mut(parent)
                        .spawn_related_scenes::<Children>(bsn_list!(Text::new("TODO:Struct")));
                    true
                }
            },
            //     ReflectRef::Struct(s) => match s.reflect_type_path() {
            //         "bevy_color::srgba::Srgba" => Some(SrgbaInspector(field.clone()).into_view()),
            //         "glam::Vec3" => Some(Vec3FieldInspector(field.clone()).into_view()),
            //         _ => Some(NestedStruct(field.clone()).into_view()),
            //     },
            ReflectRef::TupleStruct(_tuple_struct) => {
                world
                    .entity_mut(parent)
                    .spawn_related_scenes::<Children>(bsn_list!(Text::new("TODO:TupleStruct")));
                true
            }
            ReflectRef::Tuple(_tuple) => {
                world
                    .entity_mut(parent)
                    .spawn_related_scenes::<Children>(bsn_list!(Text::new("TODO:Tuple")));
                true
            }
            ReflectRef::List(_) => {
                world
                    .entity_mut(parent)
                    .spawn_related_scenes::<Children>(list_field(field));
                true
            }
            ReflectRef::Array(_array) => {
                world
                    .entity_mut(parent)
                    .spawn_related_scenes::<Children>(bsn_list!(Text::new("TODO:Array")));
                true
            }
            ReflectRef::Map(_map) => {
                world
                    .entity_mut(parent)
                    .spawn_related_scenes::<Children>(bsn_list!(Text::new("TODO:Map")));
                true
            }
            ReflectRef::Set(_set) => {
                world
                    .entity_mut(parent)
                    .spawn_related_scenes::<Children>(bsn_list!(Text::new("TODO:Set")));
                true
            }
            ReflectRef::Enum(_e) => {
                // info!("Enum: {}", e.reflect_type_path());
                // core::option::Option<bool>
                world
                    .entity_mut(parent)
                    .spawn_related_scenes::<Children>(bsn_list![
                        :field_group
                        [
                            :field_label(field),
                            Text::new("TODO:Enum")
                        ]
                    ]);
                true
            }
            ReflectRef::Opaque(partial_reflect) => {
                match partial_reflect.reflect_type_path() {
                    "bool" => {
                        world
                            .entity_mut(parent)
                            .spawn_related_scenes::<Children>(bool_field(field));
                    }
                    "f32" => {
                        if let Some(attrs) = field.attributes {
                            if let Some(range) = attrs.get::<ValueRange<f32>>() {
                                world.entity_mut(parent).spawn_related_scenes::<Children>(
                                    f32_range_field(field, range),
                                );
                                return true;
                            }
                        }

                        world
                            .entity_mut(parent)
                            .spawn_related_scenes::<Children>(f32_field(field));
                    }
                    _ => {
                        world
                            .entity_mut(parent)
                            .spawn_related_scenes::<Children>(bsn_list![
                                :field_group
                                [
                                    :field_label(field),
                                    Text::new("TODO:Opaque")
                                ]
                            ]);
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
