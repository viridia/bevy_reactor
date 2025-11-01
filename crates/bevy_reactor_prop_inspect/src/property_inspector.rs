use std::sync::Arc;

use bevy::{
    ecs::{template::template, world::DeferredWorld},
    feathers::{
        constants::fonts,
        controls::{ButtonProps, ButtonVariant, tool_button},
        font_styles::InheritableFont,
        theme::{ThemeFontColor, ThemedText},
        tokens,
    },
    prelude::*,
    reflect::{OffsetAccess, ParsedPath, ReflectKind, ReflectRef, TypeInfo},
    scene2::{Scene, SceneList, SpawnRelatedScenes, bsn, bsn_list, on},
    ui_widgets::Activate,
};
use bevy_reactor::*;

use crate::{Inspectable, InspectableRoot, InspectorFactoryRegistry};

pub fn property_inspector(subject: Arc<dyn InspectableRoot>) -> impl Scene {
    let subject_copy = subject.clone();
    bsn! {
        Node {
            display: Display::Flex,
            flex_direction: FlexDirection::Column,
            row_gap: px(4),
            min_height: px(10),
            min_width: px(10)
        }
        [
            dyn_scene(move |cx: &Cx| {
                subject.get_reflect_tracked(
                    cx, &ParsedPath(Vec::new())).unwrap().reflect_kind().to_owned()
            }, move |builder, kind| {
                match kind {
                    ReflectKind::Struct => {
                        builder.spawn_related_scenes::<Children>(
                            struct_members(Inspectable::from_root(subject_copy.clone())));
                    },
                    ReflectKind::TupleStruct => {
                        builder.spawn_related_scenes::<Children>(
                            tuple_struct_members(Inspectable::from_root(subject_copy.clone())));
                    },
                    ReflectKind::Tuple => {
                        builder.spawn_related_scenes::<Children>(
                            tuple_members(Inspectable::from_root(subject_copy.clone())));
                    },
                    ReflectKind::List => {
                        builder.spawn_related_scenes::<Children>(bsn_list!(Text("Root:List")));
                    },
                    ReflectKind::Array => {
                        builder.spawn_related_scenes::<Children>(bsn_list!(Text("Root:Array")));
                    },
                    ReflectKind::Map => {
                        builder.spawn_related_scenes::<Children>(bsn_list!(Text("Root:Map")));
                    },
                    ReflectKind::Set => {
                        builder.spawn_related_scenes::<Children>(bsn_list!(Text("Root:Set")));
                    },
                    ReflectKind::Enum => {
                        builder.spawn_related_scenes::<Children>(bsn_list!(Text("Root:Enum")));
                    },
                    ReflectKind::Opaque => {
                        builder.spawn_related_scenes::<Children>(bsn_list!(
                            Text("Root:Opaque"))
                        );
                    },
                }
            })
        ]
    }
}

fn struct_members(inspectable: Arc<Inspectable>) -> impl SceneList {
    let inspectable_copy = inspectable.clone();
    let mut fields: Vec<Box<dyn Scene>> = Vec::new();
    fields.push(Box::new(for_each(
        move |cx: &Cx| {
            // Note: this will re-run whenever the root inspectable changes.
            // However, the output will be memoized. It does mean that the list of
            // field names is regenerated with every keystroke...
            let reflect = inspectable.reflect_tracked(cx).unwrap();
            // let info = reflect.get_represented_type_info().unwrap();
            let ReflectRef::Struct(st) = reflect.reflect_ref() else {
                panic!("Expected ReflectRef::Struct")
            };
            let num_fields = st.field_len();
            let mut field_names: Vec<String> = Vec::with_capacity(num_fields);
            for findex in 0..num_fields {
                let field = st.field_at(findex).unwrap();
                // Filter out field names for fields with a value of `None`.
                if field.reflect_kind() == ReflectKind::Enum
                    && field
                        .reflect_type_path()
                        .starts_with("core::option::Option")
                {
                    let ReflectRef::Enum(enum_ref) = field.reflect_ref() else {
                        panic!("Expected ReflectRef::Enum");
                    };
                    if enum_ref.variant_name() != "None" {
                        field_names.push(st.name_at(findex).unwrap().to_string());
                    }
                } else {
                    field_names.push(st.name_at(findex).unwrap().to_string());
                }
            }
            field_names
        },
        move |parent, field_name: &String, _| {
            let world = parent.world();
            let reflect = inspectable_copy.reflect(world).unwrap();
            let info = reflect.get_represented_type_info().unwrap();
            let mut field_path = inspectable_copy.value_path.clone();
            field_path.0.push(OffsetAccess {
                access: bevy::reflect::Access::Field(std::borrow::Cow::Owned(field_name.clone())),
                offset: None,
            });
            let value_path = field_path.clone();
            let TypeInfo::Struct(st_info) = info else {
                panic!("Expected StructInfo");
            };
            let field_info = st_info.field(field_name).unwrap();
            let attrs = field_info.custom_attributes();
            let field_inspectable = Arc::new(Inspectable {
                root: inspectable_copy.root.clone(),
                name: field_name.clone(),
                field_path,
                value_path,
                can_remove: false,
                attributes: Some(attrs),
            });
            parent.spawn_related_scenes::<Children>(bsn_list!(
                :field_inspector(field_inspectable.clone())
            ));
        },
        || bsn_list!(),
    )));
    fields
}

fn tuple_struct_members(_inspectable: Arc<Inspectable>) -> impl SceneList {
    bsn_list!(Text("TupleStruct"))
}

fn tuple_members(_inspectable: Arc<Inspectable>) -> impl SceneList {
    bsn_list!(Text("Tuple"))
}

pub fn field_inspector(field: Arc<Inspectable>) -> impl Scene {
    let field_copy = field.clone();
    dyn_scene(
        move |cx: &Cx| field.reflect_tracked(cx).unwrap().reflect_kind().to_owned(),
        move |mut builder, _value| {
            let parent = builder.id();
            let world = unsafe { builder.world_mut() };

            // Unwrap `Option`
            if let Some(reflect) = field_copy.reflect(world)
                && let ReflectRef::Enum(enum_ref) = reflect.reflect_ref()
                && enum_ref
                    .reflect_type_path()
                    .starts_with("core::option::Option")
                && enum_ref.variant_name() != "None"
            {
                let mut path = field_copy.value_path.clone();
                path.0.push(OffsetAccess {
                    access: bevy::reflect::Access::TupleIndex(0),
                    offset: None,
                });

                // Create a new Inspectable for the inner value.
                let access = Arc::new(Inspectable {
                    root: field_copy.root.clone(),
                    name: field_copy.name.clone(),
                    value_path: path,
                    field_path: field_copy.value_path.clone(),
                    can_remove: true,
                    attributes: field_copy.attributes,
                });

                if world.resource_scope(|world, registry: Mut<InspectorFactoryRegistry>| {
                    registry.spawn_inspector(world, parent, access)
                }) {
                    return;
                }
            };

            world.resource_scope(|world, registry: Mut<InspectorFactoryRegistry>| {
                registry.spawn_inspector(world, parent, field_copy.clone());
            });
        },
    )
}

pub fn field_group() -> impl Scene {
    bsn! {
        Node {
            display: Display::Flex,
            flex_direction: FlexDirection::Column,
            align_items: AlignItems::Stretch,
            row_gap: px(0),
        }
    }
}

pub fn field_label(field: Arc<Inspectable>) -> impl Scene {
    let name = field.name.clone();
    let can_remove = field.can_remove;
    bsn! {
        Node
        InheritableFont {
            font: fonts::REGULAR,
            font_size: 14.0,
        }
        // ThemeFontColor(tokens::TEXT_DIM)
        ThemeFontColor(tokens::CHECKBOX_TEXT_DISABLED)
        [
            if_then(move |_: &Cx| can_remove, {
                let field = field.clone();
                move || bsn_list![:remove_button(field.clone())]
            }),
            Text({name.clone()})
            ThemedText
        ]
    }
}

pub fn remove_button(field: Arc<Inspectable>) -> impl Scene {
    bsn! {
        :tool_button(ButtonProps { variant: ButtonVariant::Normal, ..default() })
        Node {
            flex_grow: 0.0,
            height: px(16),
            padding: UiRect::axes(px(4), px(0)),
        }
        on(move |_: On<Activate>, mut world: DeferredWorld| {
            field.remove(&mut world);
        })
        [
            :icon("embedded://bevy_reactor_prop_inspect/assets/icons/x.png")
        ]
    }
}

/// Template which displays an icon.
pub fn icon(image: &'static str) -> impl Scene {
    bsn! {
        Node {
            height: Val::Px(14.0),
        }
        template(move |entity| {
            let handle = entity.resource::<AssetServer>().load(image);
            Ok(ImageNode::new(handle))
        })
    }
}
