use std::sync::Arc;

use bevy::{
    prelude::*,
    reflect::{OffsetAccess, ReflectMut, ReflectRef, TypeInfo},
    ui,
};
use bevy_mod_stylebuilder::*;
use bevy_reactor::*;
use bevy_reactor_signals::{Cx, Mutable, RunContextRead, RunContextSetup, Signal};
use obsidian_ui::{
    colors,
    controls::{DisclosureToggle, IconButton},
    size::Size,
};

use crate::{templates::field_label::FieldLabelWide, Inspectable, InspectorFactoryRegistry};

pub struct ListInspector(pub(crate) Arc<Inspectable>);

impl ViewTemplate for ListInspector {
    fn create(&self, cx: &mut Cx) -> impl IntoView {
        let field = self.0.clone();
        let expanded = cx.create_mutable(false);
        let length = cx.create_memo(move |cx| {
            if let Some(value) = field.reflect(cx) {
                return if let ReflectRef::List(list) = value.reflect_ref() {
                    list.len()
                } else {
                    0
                };
            }
            0
        });

        let field = self.0.clone();
        Fragment::new((
            FieldLabelWide::new(field.clone())
                .name(Fragment::new((
                    DisclosureToggle::new()
                        .size(Size::Xs)
                        .expanded(expanded)
                        .on_change(cx.create_callback(move |cx, value: bool| {
                            expanded.set(cx, value);
                        })),
                    TextComputed::new(move |cx| {
                        let length = length.get(cx);
                        format!("{} ({})", field.name.clone(), length)
                    }),
                )))
                .buttons(ListInspectorHeaderControls {
                    field: self.0.clone(),
                    length,
                    expanded,
                }),
            Cond::new(
                expanded.signal(),
                {
                    let field = self.0.clone();
                    move || ListElementsInspector {
                        field: field.clone(),
                        length,
                    }
                },
                || (),
            ),
        ))
    }
}

struct ListInspectorHeaderControls {
    field: Arc<Inspectable>,
    length: Signal<usize>,
    expanded: Mutable<bool>,
}

impl ViewTemplate for ListInspectorHeaderControls {
    fn create(&self, cx: &mut Cx) -> impl IntoView {
        let length = self.length;
        let expanded = self.expanded;

        let pop_disabled = cx.create_derived(move |cx| length.get(cx) == 0);

        let field = self.field.clone();
        let push = cx.create_callback(move |cx, _| {
            if let Some(list) = field.reflect(cx) {
                if let TypeInfo::List(list_type) = list.get_represented_type_info().unwrap() {
                    let registry = cx.world().resource::<AppTypeRegistry>().0.clone();
                    let registry_lock = registry.read();
                    let item_type =
                        registry_lock.get_type_data::<ReflectDefault>(list_type.item_type_id());
                    let default = item_type.unwrap().default();
                    field.update(cx, &|reflect| {
                        if let ReflectMut::List(list) = reflect.reflect_mut() {
                            list.push(default.clone_value());
                        }
                    });
                    // Auto expand when pushing.
                    expanded.set(cx, true);
                } else {
                    unreachable!("Expected List type ");
                }
            } else {
                unreachable!("Cannot push to non-list");
            }
        });

        let field = self.field.clone();
        let pop = cx.create_callback(move |cx, _| {
            field.update(cx, &|reflect| {
                if let ReflectMut::List(list) = reflect.reflect_mut() {
                    if !list.is_empty() {
                        list.pop();
                    }
                } else {
                    unreachable!("Cannot pop from non-list")
                }
            })
        });

        Fragment::new((
            IconButton::new("obsidian_ui://icons/remove.png")
                .size(Size::Xs)
                .disabled(pop_disabled)
                .minimal(true)
                .on_click(pop),
            IconButton::new("obsidian_ui://icons/add.png")
                .size(Size::Xs)
                .minimal(true)
                .on_click(push),
        ))
    }
}

struct ListElementsInspector {
    field: Arc<Inspectable>,
    length: Signal<usize>,
}

impl ViewTemplate for ListElementsInspector {
    fn create(&self, _cx: &mut Cx) -> impl IntoView {
        let field = self.field.clone();
        let length = self.length;
        Element::<Node>::new().style(style_list_items).children(
            For::index(
                move |cx| 0..length.get(cx),
                move |_, index| {
                    let mut path = field.value_path.clone();
                    path.0.push(OffsetAccess {
                        access: bevy::reflect::Access::ListIndex(index),
                        offset: None,
                    });
                    let access = Arc::new(Inspectable {
                        root: field.root.clone(),
                        name: format!("{}", index),
                        value_path: path,
                        field_path: field.value_path.clone(),
                        can_remove: false,
                        attributes: field.attributes,
                    });
                    ListItemInspector { field: access }.into_view()
                },
            )
            .with_fallback(
                Element::<Node>::new()
                    .style(style_empty_list)
                    .children("(empty list)"),
            ),
        )
    }
}

struct ListItemInspector {
    field: Arc<Inspectable>,
}

impl ViewTemplate for ListItemInspector {
    fn create(&self, cx: &mut Cx) -> impl IntoView {
        let factories = cx.read_resource::<InspectorFactoryRegistry>();
        // Either create an inspector for the field, or return an empty view.
        factories
            .create_inspector(cx, self.field.clone())
            .unwrap_or_else(|| ().into_view())
    }
}

fn style_list_items(ss: &mut StyleBuilder) {
    ss.display(ui::Display::Grid)
        .grid_auto_flow(ui::GridAutoFlow::Row)
        .grid_template_columns(vec![
            ui::RepeatedGridTrack::auto(1),
            ui::RepeatedGridTrack::flex(1, 1.),
        ])
        .column_gap(4)
        .row_gap(2)
        .align_items(ui::AlignItems::Stretch)
        .grid_column_span(2)
        .min_width(64)
        .color(colors::DIM)
        .margin_left(16);
}

fn style_empty_list(ss: &mut StyleBuilder) {
    ss.color(colors::DIM);
}
