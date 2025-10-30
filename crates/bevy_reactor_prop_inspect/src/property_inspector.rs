use std::sync::Arc;

use bevy::{
    ecs::template::template,
    prelude::*,
    reflect::{OffsetAccess, ParsedPath, ReflectKind, ReflectRef, TypeInfo},
    scene2::{Scene, SceneList, SpawnRelatedScenes, bsn, bsn_list},
};
use bevy_reactor::*;

use crate::{Inspectable, InspectableRoot, InspectorFactoryRegistry};

pub fn property_inspector(subject: Arc<dyn InspectableRoot>) -> impl Scene {
    let subject_copy = subject.clone();
    let subject_copy2 = subject.clone();
    bsn! {
        Node {
            display: Display::Flex,
            flex_direction: FlexDirection::Column,
            row_gap: px(2),
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
                        builder.spawn_related_scenes::<Children>(bsn_list!(Text("List")));
                    },
                    ReflectKind::Array => {
                        builder.spawn_related_scenes::<Children>(bsn_list!(Text("Array")));
                    },
                    ReflectKind::Map => {
                        builder.spawn_related_scenes::<Children>(bsn_list!(Text("Map")));
                    },
                    ReflectKind::Set => {
                        builder.spawn_related_scenes::<Children>(bsn_list!(Text("Set")));
                    },
                    ReflectKind::Enum => {
                        builder.spawn_related_scenes::<Children>(bsn_list!(Text("Enum")));
                    },
                    ReflectKind::Opaque => {
                        builder.spawn_related_scenes::<Children>(bsn_list!(Text("Opaque")));
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
            // However, the output will be memoized.
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
            world.resource_scope(|world, registry: Mut<InspectorFactoryRegistry>| {
                registry.spawn_inspector(world, parent, field_copy.clone());
            });
        },
    )
}

// / Displays a inspector panel card with a title and a body.
// #[derive(Clone, Default)]
// pub struct PropertyInspector<Title: SceneList> {
//     /// The content of the title section.
//     pub title: ChildArray,
//     /// The content of the body section.
//     pub body: ChildArray,
//     /// Whether the panel is expanded or not. When collapsed, only the title is shown.
//     pub expanded: Signal<bool>,
// }

// impl PropertyInspector {
//     /// Create a new inspector panel with the given title and body.
//     pub fn new() -> Self {
//         Self::default()
//     }

//     /// Set the title of the inspector panel.
//     pub fn title<V: ChildViewTuple>(mut self, title: V) -> Self {
//         self.title = title.to_child_array();
//         self
//     }

//     /// Set the body of the inspector panel.
//     pub fn body<V: ChildViewTuple>(mut self, body: V) -> Self {
//         self.body = body.to_child_array();
//         self
//     }

//     /// Set the expanded signal of the inspector panel.
//     pub fn expanded(mut self, expanded: impl IntoSignal<bool>) -> Self {
//         self.expanded = expanded.into_signal();
//         self
//     }
// }

// impl ViewTemplate for PropertyInspector {
//     fn create(&self, _cx: &mut Cx) -> impl IntoView {
//         let expanded = self.expanded;
//         let body = self.body.clone();
//         Element::<Node>::new()
//             .style(style_inspector_panel)
//             .children((
//                 Element::<Node>::new()
//                     .style((typography::text_default, style_inspector_panel_header))
//                     .children(self.title.clone()),
//                 Cond::new(
//                     expanded,
//                     move || {
//                         Element::<Node>::new()
//                             .style(style_inspector_panel_body)
//                             .children(body.clone())
//                     },
//                     || (),
//                 ),
//             ))
//     }
// }
