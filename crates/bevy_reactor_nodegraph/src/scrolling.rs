use bevy::{
    color::Luminance,
    ecs::{
        component::Component,
        hierarchy::{ChildOf, Children},
        lifecycle::Insert,
        observer::On,
        query::{With, Without},
        reflect::ReflectComponent,
        system::{Query, Res},
        template::GetTemplate,
    },
    feathers::{cursor::EntityCursor, palette},
    log::warn_once,
    math::{Rect, Vec2},
    picking::{
        events::{Cancel, Drag, DragEnd, DragStart, Pointer, Press},
        hover::Hovered,
    },
    reflect::{Reflect, prelude::ReflectDefault},
    scene2::{Scene, bsn, on},
    ui::{
        AlignSelf, BackgroundColor, BorderRadius, ComputedNode, ComputedUiRenderTargetInfo,
        JustifySelf, Node, PositionType, UiGlobalTransform, UiRect, UiScale, Val, ZIndex, px,
    },
    ui_widgets::ControlOrientation,
};

use crate::{Graph, GraphBounds, GraphDocument};

pub fn node_graph_scrollbar(orientation: ControlOrientation) -> impl Scene {
    bsn! {
        Node {
            align_self: AlignSelf::Stretch,
            justify_self: JustifySelf::Stretch,
        }
        GraphScrollbar {
            orientation: orientation
        }
        EntityCursor::System(bevy::window::SystemCursorIcon::Default)
        ZIndex(10)
        on(scrollbar_on_track_press)
        [
            Node {
                position_type: PositionType::Absolute,
                left: px(0),
                top: px(0),
                bottom: px(0),
                right: px(0),
                border: UiRect::all(px(3)),
                border_radius: BorderRadius::all(px(6))
            }
            GraphScrollbarThumb
            BackgroundColor(palette::GRAY_2)
            Hovered::default()
            EntityCursor::System(bevy::window::SystemCursorIcon::Pointer)
            on(on_change_hover)
            // on(on_change_dragging)
            on(scrollbar_thumb_on_thumb_press)
            on(scrollbar_on_drag_start)
            on(scrollbar_on_drag)
            on(scrollbar_on_drag_end)
            on(scrollbar_on_drag_cancel)
        ]
    }
}

#[derive(Component, Debug, Reflect, Clone, GetTemplate)]
#[reflect(Component)]
pub struct GraphScrollbar {
    /// Whether the scrollbar is vertical or horizontal.
    pub orientation: ControlOrientation,
}

/// Marker component to indicate that the entity is a scrollbar thumb (the moving, draggable part of
/// the scrollbar). This should be a child of the scrollbar entity.
#[derive(Component, Debug, Default, Clone)]
#[require(ScrollbarDragState)]
#[derive(Reflect)]
#[reflect(Component)]
pub struct GraphScrollbarThumb;

/// Component used to manage the state of a scrollbar during dragging. This component is
/// inserted on the thumb entity.
#[derive(Component, Default, Reflect)]
#[reflect(Component, Default)]
struct ScrollbarDragState {
    /// Whether the scrollbar is currently being dragged.
    pub dragging: bool,
    /// The value of the scrollbar when dragging started.
    drag_origin: f32,
}

fn on_change_hover(
    change: On<Insert, Hovered>,
    mut q_thumb: Query<(&Hovered, &ScrollbarDragState, &mut BackgroundColor)>,
) {
    if let Ok((hovered, drag_state, mut bg_color)) = q_thumb.get_mut(change.entity) {
        update_thumb_colors(hovered, drag_state, &mut bg_color);
    }
}

fn on_change_dragging(
    change: On<Insert, ScrollbarDragState>,
    mut q_thumb: Query<(&Hovered, &ScrollbarDragState, &mut BackgroundColor)>,
) {
    if let Ok((hovered, drag_state, mut bg_color)) = q_thumb.get_mut(change.entity) {
        update_thumb_colors(hovered, drag_state, &mut bg_color);
    }
}

fn update_thumb_colors(
    hovered: &Hovered,
    drag_state: &ScrollbarDragState,
    bg_color: &mut BackgroundColor,
) {
    if hovered.0 || drag_state.dragging {
        bg_color.0 = palette::GRAY_2.lighter(0.1);
    } else {
        bg_color.0 = palette::GRAY_2;
    }
}

fn scrollbar_on_track_press(
    mut ev: On<Pointer<Press>>,
    mut q_scrollbar: Query<(
        &GraphScrollbar,
        &ComputedNode,
        &ComputedUiRenderTargetInfo,
        &UiGlobalTransform,
        &ChildOf,
    )>,
    q_graph: Query<(&ComputedNode, &GraphBounds, &Children), With<Graph>>,
    mut q_doc: Query<&mut Node, (With<GraphDocument>, Without<Graph>)>,
    ui_scale: Res<UiScale>,
) {
    if let Ok((scrollbar, node, node_target, transform, &ChildOf(parent))) =
        q_scrollbar.get_mut(ev.entity)
    {
        // If they click on the scrollbar track, page up or down.
        ev.propagate(false);

        // Find the graph
        let Ok((graph_computed_node, &GraphBounds(bounds), graph_children)) = q_graph.get(parent)
        else {
            warn_once!("Graph not found");
            return;
        };

        // Find doc element
        let Some(doc_ent) = graph_children.iter().find(|&&ent| q_doc.contains(ent)) else {
            warn_once!("Document not found");
            return;
        };
        let mut doc_node = q_doc.get_mut(*doc_ent).unwrap();

        // Convert to widget-local coordinates.
        let local_pos = transform.try_inverse().unwrap().transform_point2(
            ev.event().pointer_location.position * node_target.scale_factor() / ui_scale.0,
        ) + node.size() * 0.5;

        // Current scroll position relative to document bounds.
        let mut scroll_pos = scroll_pos_from_node(&doc_node);

        // Size of the visible scrolling area.
        let visible_size = graph_computed_node.size() * graph_computed_node.inverse_scale_factor;

        // Min and max scrolling range
        let scroll_limits = Rect {
            min: bounds.min,
            max: (bounds.max - visible_size).max(bounds.min),
        };

        fn adjust_scroll_pos(scroll_pos: &mut f32, click_pos: f32, step: f32, min: f32, max: f32) {
            *scroll_pos =
                (*scroll_pos + if click_pos > *scroll_pos { step } else { -step }).clamp(min, max);
        }

        match scrollbar.orientation {
            ControlOrientation::Horizontal => {
                if node.size().x > 0. {
                    let click_pos = local_pos.x * bounds.width() / node.size().x;
                    adjust_scroll_pos(
                        &mut scroll_pos.x,
                        click_pos,
                        visible_size.x,
                        scroll_limits.min.x,
                        scroll_limits.max.x,
                    );
                }
            }
            ControlOrientation::Vertical => {
                if node.size().y > 0. {
                    let click_pos = local_pos.y * bounds.height() / node.size().y;
                    adjust_scroll_pos(
                        &mut scroll_pos.y,
                        click_pos,
                        visible_size.y,
                        scroll_limits.min.y,
                        scroll_limits.max.y,
                    );
                }
            }
        }

        doc_node.left = px(-scroll_pos.x);
        doc_node.top = px(-scroll_pos.y);
    }
}

fn scrollbar_thumb_on_thumb_press(mut ev: On<Pointer<Press>>) {
    // If they click on the thumb, do nothing. This will be handled by the drag event.
    ev.propagate(false);
}

fn scrollbar_on_drag_start(
    mut ev: On<Pointer<DragStart>>,
    mut q_thumb: Query<(&ChildOf, &mut ScrollbarDragState), With<GraphScrollbarThumb>>,
    q_scrollbar: Query<(&GraphScrollbar, &ChildOf)>,
    q_graph: Query<(&GraphBounds, &Children), With<Graph>>,
    q_doc: Query<&Node, (With<GraphDocument>, Without<Graph>)>,
) {
    if let Ok((&ChildOf(thumb_parent), mut drag)) = q_thumb.get_mut(ev.entity) {
        ev.propagate(false);
        if let Ok((scrollbar, &ChildOf(scrollbar_parent))) = q_scrollbar.get(thumb_parent)
            && let Ok((&GraphBounds(bounds), graph_children)) = q_graph.get(scrollbar_parent)
        {
            // Find doc element
            let Some(doc_ent) = graph_children.iter().find(|&&ent| q_doc.contains(ent)) else {
                warn_once!("Document not found");
                return;
            };
            let doc_node = q_doc.get(*doc_ent).unwrap();
            let scroll_pos = scroll_pos_from_node(doc_node) - bounds.min;

            drag.dragging = true;
            drag.drag_origin = match scrollbar.orientation {
                ControlOrientation::Horizontal => scroll_pos.x,
                ControlOrientation::Vertical => scroll_pos.y,
            };
        }
    }
}

fn scrollbar_on_drag(
    mut ev: On<Pointer<Drag>>,
    mut q_thumb: Query<(&ChildOf, &mut ScrollbarDragState), With<GraphScrollbarThumb>>,
    mut q_scrollbar: Query<(&ComputedNode, &GraphScrollbar, &ChildOf)>,
    q_graph: Query<(&ComputedNode, &GraphBounds, &Children), With<Graph>>,
    mut q_doc: Query<&mut Node, (With<GraphDocument>, Without<Graph>)>,
    // mut q_scroll_pos: Query<(&mut ScrollPosition, &ComputedNode), Without<GraphScrollbar>>,
    ui_scale: Res<UiScale>,
) {
    if let Ok((ChildOf(thumb_parent), drag)) = q_thumb.get_mut(ev.entity)
        && let Ok((node, scrollbar, &ChildOf(scrollbar_parent))) =
            q_scrollbar.get_mut(*thumb_parent)
        && let Ok((graph_computed_node, &GraphBounds(bounds), graph_children)) =
            q_graph.get(scrollbar_parent)
    {
        ev.propagate(false);
        // Find doc element
        let Some(doc_ent) = graph_children.iter().find(|&&ent| q_doc.contains(ent)) else {
            warn_once!("Document not found");
            return;
        };
        if drag.dragging {
            let mut doc_node = q_doc.get_mut(*doc_ent).unwrap();
            let scroll_pos = scroll_pos_from_node(doc_node.as_ref());
            let mut next_scroll_pos = scroll_pos;

            let distance = ev.event().distance / ui_scale.0;
            // Size of the visible scrolling area.
            let visible_size =
                graph_computed_node.size() * graph_computed_node.inverse_scale_factor;
            let content_size = bounds.size();
            let scrollbar_size = (node.size() * node.inverse_scale_factor).max(Vec2::ONE);

            // Min and max scrolling range
            let scroll_limits = Rect {
                min: bounds.min,
                max: (bounds.max - visible_size).max(bounds.min),
            };

            match scrollbar.orientation {
                ControlOrientation::Horizontal => {
                    next_scroll_pos.x = (drag.drag_origin
                        + (distance.x * content_size.x) / scrollbar_size.x)
                        .clamp(scroll_limits.min.x, scroll_limits.max.x);
                }
                ControlOrientation::Vertical => {
                    next_scroll_pos.y = (drag.drag_origin
                        + (distance.y * content_size.y) / scrollbar_size.y)
                        .clamp(scroll_limits.min.y, scroll_limits.max.y);
                }
            };

            if next_scroll_pos != scroll_pos {
                doc_node.left = px(-next_scroll_pos.x);
                doc_node.top = px(-next_scroll_pos.y);
            }
        }
    }
}

fn scrollbar_on_drag_end(
    mut drag_end: On<Pointer<DragEnd>>,
    mut q_thumb: Query<&mut ScrollbarDragState, With<GraphScrollbarThumb>>,
) {
    if let Ok(mut drag) = q_thumb.get_mut(drag_end.entity) {
        drag_end.propagate(false);
        if drag.dragging {
            drag.dragging = false;
        }
    }
}

fn scrollbar_on_drag_cancel(
    mut cancel: On<Pointer<Cancel>>,
    mut q_thumb: Query<&mut ScrollbarDragState, With<GraphScrollbarThumb>>,
) {
    if let Ok(mut drag) = q_thumb.get_mut(cancel.entity) {
        cancel.propagate(false);
        if drag.dragging {
            drag.dragging = false;
        }
    }
}

const MIN_SCROLL_SIZE: f32 = 8.0;

#[allow(clippy::type_complexity)]
pub(crate) fn update_scrollbar_thumb(
    q_graph: Query<(&ComputedNode, &GraphBounds, &Children), With<Graph>>,
    q_doc: Query<&Node, (With<GraphDocument>, Without<Graph>)>,
    q_scrollbar: Query<(&GraphScrollbar, &ComputedNode, &ChildOf, &Children), Without<Graph>>,
    mut q_thumb: Query<
        &mut Node,
        (
            With<GraphScrollbarThumb>,
            Without<Graph>,
            Without<GraphDocument>,
        ),
    >,
) {
    for (scrollbar, scrollbar_node, &ChildOf(parent), children) in q_scrollbar.iter() {
        // Find the graph
        let Ok((graph_computed_node, &GraphBounds(bounds), graph_children)) = q_graph.get(parent)
        else {
            warn_once!("Graph not found");
            continue;
        };

        // Find doc element
        let Some(doc_node) = graph_children.iter().find_map(|&ent| q_doc.get(ent).ok()) else {
            warn_once!("Document not found");
            continue;
        };

        // Size of the visible scrolling area.
        let visible_size = graph_computed_node.size() * graph_computed_node.inverse_scale_factor;

        // Length of the scrollbar track.
        let track_length = scrollbar_node.size() * scrollbar_node.inverse_scale_factor;

        // Current scroll position relative to document bounds.
        let scroll_pos = scroll_pos_from_node(doc_node) - bounds.min;

        fn size_and_pos(
            content_size: f32,
            visible_size: f32,
            track_length: f32,
            min_size: f32,
            offset: f32,
        ) -> (f32, f32) {
            let thumb_size = if content_size > visible_size {
                (track_length * visible_size / content_size)
                    .max(min_size)
                    .min(track_length)
            } else {
                track_length
            };

            let thumb_pos = if content_size > visible_size {
                offset * (track_length - thumb_size) / (content_size - visible_size)
            } else {
                0.
            };

            (thumb_size, thumb_pos)
        }

        for child in children {
            if let Ok(mut thumb) = q_thumb.get_mut(*child) {
                match scrollbar.orientation {
                    ControlOrientation::Horizontal => {
                        let (thumb_size, thumb_pos) = size_and_pos(
                            bounds.width(),
                            visible_size.x,
                            track_length.x,
                            MIN_SCROLL_SIZE,
                            scroll_pos.x,
                        );

                        thumb.top = Val::Px(0.);
                        thumb.bottom = Val::Px(0.);
                        thumb.left = Val::Px(thumb_pos);
                        thumb.width = Val::Px(thumb_size);
                    }
                    ControlOrientation::Vertical => {
                        let (thumb_size, thumb_pos) = size_and_pos(
                            bounds.height(),
                            visible_size.y,
                            track_length.y,
                            MIN_SCROLL_SIZE,
                            scroll_pos.y,
                        );

                        thumb.left = Val::Px(0.);
                        thumb.right = Val::Px(0.);
                        thumb.top = Val::Px(thumb_pos);
                        thumb.height = Val::Px(thumb_size);
                    }
                };
            }
        }
    }
}

fn scroll_pos_from_node(node: &Node) -> Vec2 {
    // Current scroll position relative to document bounds.
    Vec2::new(
        match node.left {
            Val::Px(px) => -px,
            _ => 0.0,
        },
        match node.top {
            Val::Px(px) => -px,
            _ => 0.0,
        },
    )
}
