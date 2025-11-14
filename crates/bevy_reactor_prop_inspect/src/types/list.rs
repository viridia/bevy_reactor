use std::sync::Arc;

use bevy::color::Color;
use bevy::ecs::hierarchy::Children;
use bevy::prelude::Name;
use bevy::reflect::OffsetAccess;
use bevy::scene2::SpawnRelatedScenes;
use bevy::ui::{AlignSelf, BackgroundColor, UiRect, Val};
use bevy::{
    ecs::{observer::On, reflect::AppTypeRegistry, world::DeferredWorld},
    feathers::{
        constants::fonts,
        font_styles::InheritableFont,
        theme::{ThemeFontColor, ThemedText},
        tokens,
    },
    log::warn,
    reflect::{ReflectMut, ReflectRef, TypeInfo, prelude::ReflectDefault},
    scene2::{SceneList, bsn_list, on},
    ui::{AlignItems, Display, FlexDirection, JustifyContent, Node, px, widget::Text},
    ui_widgets::{Activate, checkbox_self_update},
};
use bevy_reactor::{Cx, effect, for_each, if_then};

use crate::property_inspector::field_inspector;
use crate::{
    Inspectable, disclosure_toggle,
    property_inspector::{add_button, field_group, field_label, flex_spacer},
};

pub fn list_field(field: Arc<Inspectable>) -> impl SceneList {
    let field_copy = field.clone();
    let field_copy2 = field.clone();
    let field_copy3 = field.clone();
    let field_copy4 = field.clone();
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
                :disclosure_toggle
                #toggle
                on(checkbox_self_update),

                Text("")
                ThemedText
                effect::memo_effect(move |cx: &Cx| {
                    let reflect = field_copy.reflect_tracked(cx).unwrap();
                    reflect.reflect_short_type_path().to_owned()
                }, |entity, path| {
                    if let Some(mut text) = entity.get_mut::<Text>() {
                        text.0.clone_from(path);
                    }
                }),

                Text("[]")
                ThemedText
                effect::memo_effect(move |cx: &Cx| {
                    let reflect = field_copy2.reflect_tracked(cx).unwrap();
                    if let ReflectRef::List(value) = reflect.reflect_ref() {
                        return value.len();
                    }
                    0
                }, |entity, x| {
                    if let Some(mut text) = entity.get_mut::<Text>() {
                        text.0 = format!("[{x}]");
                    }
                }),

                :flex_spacer,

                :add_button()
                on(move |_: On<Activate>, mut world: DeferredWorld| {
                    if let Some(list) = field_copy3.reflect(&world) {
                        if let TypeInfo::List(list_type) = list.get_represented_type_info().unwrap() {
                            let item_type = list_type.item_ty();
                            let registry = world.resource::<AppTypeRegistry>().0.clone();
                            let registry_lock = registry.read();
                            let item_type =
                                registry_lock.get_type_data::<ReflectDefault>(item_type.id());
                            let default = item_type.unwrap().default();
                            field_copy3.update_value(&mut world, &|reflect| {
                                if let ReflectMut::List(list) = reflect.reflect_mut() {
                                    if let Ok(clone) = default.reflect_clone() {
                                        list.push(clone);
                                    } else {
                                        warn!("List data could not be cloned");
                                    }
                                }
                            });
                            // Auto expand when pushing.
                            // expanded.set(cx, true);
                        } else {
                            unreachable!("Expected List type ");
                        }
                    } else {
                        unreachable!("Cannot push to non-list");
                    }
                })
            ],

            if_then(|_cx: &Cx| {
                // TODO: Return true if list is expanded.
                // cx.entity(#toggle).contains::<Checked>();
                true
            }, move || {
                let field = field_copy4.clone();
                let field2 = field_copy4.clone();
                bsn_list!(
                    Node {
                        display: Display::Flex,
                        flex_direction: FlexDirection::Row
                    }
                    [
                        Node {
                            width: px(2),
                            align_self: AlignSelf::Stretch,
                            margin: UiRect::axes(px(4), Val::ZERO),
                        }
                        BackgroundColor(Color::WHITE),

                        Node {
                            display: Display::Flex,
                            flex_direction: FlexDirection::Column,
                            flex_grow: 1.0,
                            flex_shrink: 1.0,
                            flex_basis: px(0),
                        } [
                            for_each(move |cx: &Cx| {
                                let reflect = field.reflect_tracked(cx).unwrap();
                                if let ReflectRef::List(value) = reflect.reflect_ref() {
                                    return (0..value.len()).collect();
                                }
                                Vec::default()
                            }, move |parent, index, _| {
                                let item_index = *index;
                                let mut item_path = field2.value_path.clone();
                                item_path.0.push(OffsetAccess {
                                    access: bevy::reflect::Access::ListIndex(item_index),
                                    offset: None,
                                });
                                let item_inspectable = Arc::new(Inspectable {
                                    root: field2.root.clone(),
                                    name: format!("{item_index}"),
                                    field_path: field2.value_path.clone(),
                                    value_path: item_path,
                                    can_remove: true,
                                    can_move: true,
                                    attributes: field2.attributes,
                                });
                                parent.spawn_related_scenes::<Children>(bsn_list!(
                                    :field_inspector(item_inspectable.clone())
                                ));
                            }, || ()),
                        ]
                    ]
                )}
            )
        ]
    ]
}
