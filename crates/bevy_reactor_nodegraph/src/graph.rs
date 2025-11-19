use std::ops::Mul;

use bevy::{
    asset::Assets,
    camera::visibility::Visibility,
    color::{Alpha, Color},
    ecs::{
        component::Component,
        entity::Entity,
        hierarchy::{ChildOf, Children},
        lifecycle::Insert,
        observer::On,
        query::{Changed, Or, With, Without},
        system::{Commands, Query, Res, ResMut},
        template::GetTemplate,
    },
    feathers::{
        constants::fonts, cursor::EntityCursor, font_styles::InheritableFont, palette,
        theme::ThemedText,
    },
    input::{ButtonInput, keyboard::KeyCode, mouse::MouseScrollUnit},
    log::{debug, warn_once},
    math::{Rect, Vec2},
    picking::{
        Pickable,
        events::{
            Cancel, Drag, DragDrop, DragEnd, DragEnter, DragLeave, DragStart, Pointer, Press,
            Scroll,
        },
        hover::Hovered,
    },
    render::storage::ShaderStorageBuffer,
    scene2::{Scene, bsn, on, template_value},
    ui::{
        AlignItems, AlignSelf, BackgroundColor, BorderColor, BorderRadius, BoxShadow, ComputedNode,
        ComputedUiRenderTargetInfo, Display, FlexDirection, GlobalZIndex, GridPlacement,
        JustifyContent, Node, Outline, PositionType, RepeatedGridTrack, UiGlobalTransform, UiRect,
        UiTransform, Val, Val2, px, widget::Text,
    },
    ui_render::prelude::MaterialNode,
    ui_widgets::ControlOrientation,
};

use crate::{
    ConnectEvent, ConnectionAnchor, ConnectionTarget, DragAction, DrawPathMaterial, DrawablePath,
    Gesture, GestureState, MoveNodesEvent, scrolling::node_graph_scrollbar,
};

/// A node graph.
#[derive(Component, Clone, Default)]
#[require(GraphBounds)]
pub struct Graph;

/// Calculated bounds of the graph's contents.
#[derive(Component, Clone, Default)]
pub struct GraphBounds(pub Rect);

/// Scrolling content region of the graph.
#[derive(Component, Clone, Default)]
pub struct GraphDocument;

/// A node within a node graph.
#[derive(Component, Clone, Default)]
#[require(GraphNodeOffset, GraphNodeSelection, GlobalZIndex(3))]
pub struct GraphNode;

/// Base offset for dragging a node.
#[derive(Component, Clone, Default)]
pub struct GraphNodeOffset(pub Vec2);

/// Marker that indicates whether this node is selected.
#[derive(Component, Clone, Default)]
pub struct GraphNodeSelection {
    /// Node is selected.
    pub selected: bool,
    /// Node will be selected as the result of a pending selection gesture.
    pub pending: bool,
}

impl GraphNodeSelection {
    pub fn show_selection(&self) -> bool {
        self.selected || self.pending
    }
}

/// Marker for the selection rectangle gesture highlight.
#[derive(Component, Clone, Default)]
pub struct SelectionRect;

pub fn node_graph() -> impl Scene {
    bsn! {
        Graph
        Node {
            display: Display::Grid,
            grid_template_rows: {vec![
                RepeatedGridTrack::flex(1, 1.),
                RepeatedGridTrack::px(1, 12.0),
            ]},
            grid_template_columns: {vec![
                RepeatedGridTrack::flex(1, 1.),
                RepeatedGridTrack::px(1, 12.0),
            ]},
            // border: UiRect::new(px(0), px(2), px(0), px(2)),
        }
        EntityCursor::System(bevy::window::SystemCursorIcon::Crosshair)
        on(on_graph_press)
        on(on_graph_drag_start)
        on(on_graph_drag)
        on(on_graph_drag_end)
        on(on_graph_drag_cancel)
        on(on_graph_scroll)
        [
            // Vertical scrollbar
            :node_graph_scrollbar(ControlOrientation::Vertical)
            Node {
                grid_column: GridPlacement::start_span(2, 1),
                grid_row: GridPlacement::start_span(1, 1),
            }
            ,
            // Horizontal scrollbar
            :node_graph_scrollbar(ControlOrientation::Horizontal)
            Node {
                grid_column: GridPlacement::start_span(1, 1),
                grid_row: GridPlacement::start_span(2, 1),
            }
        ]
    }
}

pub fn node_graph_document() -> impl Scene {
    bsn! {
        GraphDocument
        Node {
            position_type: PositionType::Absolute,
            left: px(0.0),
            top: px(0.0),
        }
        [
            :selection_rect()
            Node {
                width: px(100),
                height: px(100),
            }
        ]
    }
}

pub fn node_graph_node(position: Vec2) -> impl Scene {
    bsn! {
        Node {
            position_type: PositionType::Absolute,
            display: Display::Flex,
            flex_direction: FlexDirection::Column,
            justify_content: JustifyContent::Start,
            align_items: AlignItems::Stretch,
            left: px(position.x),
            top: px(position.y),
            min_width: px(70),
            min_height: px(20),
        }
        GraphNode
        Hovered::default()
        BorderRadius::all(px(3))
        BorderColor::all(palette::GRAY_3)
        BoxShadow::new(
            Color::BLACK,
            px(0.),
            px(0.),
            px(3),
            px(3),
        )
        on(on_graph_node_press)
        on(on_graph_node_drag_start)
        on(on_graph_node_drag)
        on(on_graph_node_drag_end)
        on(on_graph_node_drag_cancel)
        UiTransform {
            translation: Val2::new(
                Val::Percent(-50.0),
                Val::Percent(-50.0),
            )
        }
    }
}

/// Displayed title of a graph node.
#[derive(Component, Default, Clone)]
pub struct GraphNodeTitle;

pub fn node_graph_node_title() -> impl Scene {
    bsn! {
        Node {
            display: Display::Flex,
            flex_direction: FlexDirection::Row,
            min_height: px(24),
            border: {UiRect::ZERO.with_bottom(px(1.0))},
            justify_content: JustifyContent::Start,
            align_items: AlignItems::Center,
            padding: UiRect::axes(px(8.0), px(2.0)),
        }
        EntityCursor::System(bevy::window::SystemCursorIcon::Move)
        GraphNodeTitle
        BorderRadius::new(px(3), px(3), px(0), px(0))
        BorderColor::all(Color::WHITE)
        BackgroundColor(palette::ACCENT)
        BorderColor::all(palette::GRAY_3)
        InheritableFont {
            font: fonts::REGULAR,
            font_size: 12.0,
        }
    }
}

/// Identifies the entity containing the node's content, such as widgets.
#[derive(Component, Default, Clone)]
pub struct GraphNodeBody;

pub fn node_graph_node_body() -> impl Scene {
    bsn! {
        Node {
            display: Display::Flex,
            flex_direction: FlexDirection::Column,
            justify_content: JustifyContent::Start,
            align_items: AlignItems::Start,
            padding: UiRect::axes(px(8.0), px(4.0)),
            row_gap: px(4),
        }
        BorderRadius::new(px(0), px(0), px(3), px(3))
        BackgroundColor(palette::GRAY_2)
        GraphNodeBody
        EntityCursor::System(bevy::window::SystemCursorIcon::Default)
        InheritableFont {
            font: fonts::REGULAR,
            font_size: 12.0,
        }
    }
}

pub fn input_terminal(color: Color) -> impl Scene {
    bsn! {
        Node {
            display: Display::Flex,
            flex_direction: FlexDirection::Row,
            justify_content: JustifyContent::End,
            align_items: AlignItems::Center,
            align_self: AlignSelf::Stretch,
            min_height: px(16),
        }
        InheritableFont {
            font: fonts::REGULAR,
            font_size: 12.0,
        }
        EntityCursor::System(bevy::window::SystemCursorIcon::Pointer)
        [
            Node {
                position_type: PositionType::Absolute,
                width: px(12),
                height: px(12),
                border: UiRect::all(px(3)), // Invisible border increases picking area
                left: px(-14),
            }
            template_value(Terminal::Input)
            BorderRadius::MAX
            BackgroundColor(color)
            on(on_terminal_drag_start)
            on(on_terminal_drag)
            on(on_terminal_drag_end)
            on(on_terminal_drag_cancel)
            on(on_terminal_drag_enter)
            on(on_terminal_drag_leave)
            on(on_terminal_drop)
        ]
    }
}

pub fn output_terminal(color: Color) -> impl Scene {
    bsn! {
        Node {
            display: Display::Flex,
            flex_direction: FlexDirection::Row,
            justify_content: JustifyContent::End,
            align_items: AlignItems::Center,
            align_self: AlignSelf::End,
            min_height: px(16),
        }
        InheritableFont {
            font: fonts::REGULAR,
            font_size: 12.0,
        }
        EntityCursor::System(bevy::window::SystemCursorIcon::Pointer)
        [
            Node {
                position_type: PositionType::Absolute,
                width: px(12),
                height: px(12),
                right: px(-14),
                border: UiRect::all(px(3)), // Invisible border increases picking area
            }
            template_value(Terminal::Output)
            BorderRadius::MAX
            BackgroundColor(color)
            on(on_terminal_drag_start)
            on(on_terminal_drag)
            on(on_terminal_drag_end)
            on(on_terminal_drag_cancel)
            on(on_terminal_drag_enter)
            on(on_terminal_drag_leave)
            on(on_terminal_drop)
        ]
    }
}

pub fn selection_rect() -> impl Scene {
    bsn! {
        Node {
            position_type: PositionType::Absolute,
            border: UiRect::all(px(1)),
            min_height: px(16),
        }
        SelectionRect
        BorderRadius::all(px(3))
        BorderColor::all(palette::ACCENT.with_alpha(0.2))
        BackgroundColor({palette::ACCENT.with_alpha(0.03)})
        template_value(Visibility::Hidden)
        Pickable::IGNORE
    }
}

pub fn label(text: impl Into<String>) -> impl Scene {
    let text = text.into();
    bsn! {
        Text({text.clone()})
        ThemedText
    }
}

/// A terminal where connections can attach to.
#[derive(Component, Clone, Default, Debug, PartialEq)]
pub enum Terminal {
    /// An input terminal that connects to an connection dst
    #[default]
    Input,
    /// An output terminal that connects to an connection src
    Output,
}

/// For a connection drag gesture, the current drag location - what we are hovering over.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ConnectionTerminus {
    /// Input or output terminal
    Terminal(Entity),
    /// A location (used while dragging).
    Location(Vec2),
}

impl Default for ConnectionTerminus {
    fn default() -> Self {
        ConnectionTerminus::Location(Vec2::ZERO)
    }
}

/// Displays a stroked path between two nodes.
#[derive(Component, GetTemplate, Clone)]
#[component(immutable)]
#[require(Node { ..Default::default() }, Pickable::IGNORE)]
pub struct Connection {
    /// Pixel position of the source terminal.
    pub src: ConnectionTerminus,

    /// Pixel position of the destination terminal.
    pub dst: ConnectionTerminus,

    /// Color of this connection
    pub color: Color,
}

/// Used for picking connections
#[derive(Component, Clone, Debug)]
pub enum ConnectionHitBox {
    Src,
    Dst,
}

/// Update shader uniforms for connection
#[allow(clippy::type_complexity)]
#[allow(clippy::too_many_arguments)]
pub(crate) fn on_insert_connection(
    insert: On<Insert, Connection>,
    mut q_connection: Query<(
        &Connection,
        &mut Node,
        Option<&MaterialNode<DrawPathMaterial>>,
        Option<&Children>,
    )>,
    mut q_connection_hit: Query<(&ConnectionHitBox, &mut Node), Without<Connection>>,
    q_parent: Query<&ChildOf>,
    q_graph_doc: Query<(&UiGlobalTransform, &ComputedNode), With<GraphDocument>>,
    q_terminals: Query<(&UiGlobalTransform, &ComputedNode)>,
    mut r_materials: ResMut<Assets<DrawPathMaterial>>,
    mut r_bindings: ResMut<Assets<ShaderStorageBuffer>>,
    mut commands: Commands,
) {
    if let Ok((connection, mut node, material_node, children)) = q_connection.get_mut(insert.entity)
    {
        let Some((doc_transform, doc_computed_node)) = q_parent
            .iter_ancestors(insert.entity)
            .find_map(|ent| q_graph_doc.get(ent).ok())
        else {
            return;
        };

        let doc_origin = doc_transform.translation - doc_computed_node.size() * 0.5;

        let mut path = DrawablePath::new(connection.color.into(), 1.5);
        let src = match connection.src {
            ConnectionTerminus::Terminal(entity) => {
                if let Ok((transform, computed_node)) = q_terminals.get(entity) {
                    (transform.translation - doc_origin) * computed_node.inverse_scale_factor()
                } else {
                    Vec2::ZERO
                }
            }
            ConnectionTerminus::Location(pos) => pos,
        };
        let dst = match connection.dst {
            ConnectionTerminus::Terminal(entity) => {
                if let Ok((transform, computed_node)) = q_terminals.get(entity) {
                    (transform.translation - doc_origin) * computed_node.inverse_scale_factor()
                } else {
                    Vec2::ZERO
                }
            }
            ConnectionTerminus::Location(pos) => pos,
        };
        let dx = (dst.x - src.x).abs().mul(0.3).min(20.);
        let dy = dst.y - src.y;
        let src1 = src + Vec2::new(dx, 0.);
        let dst1 = dst - Vec2::new(dx, 0.);
        path.move_to(src);
        let mlen = src1.distance(dst1);
        if dy.abs() < 0.01 {
            // Workaround for a bug in shader when quadratics are colinear.
            path.line_to(dst);
        } else if mlen > 40. {
            let src2 = src1.lerp(dst1, 20. / mlen);
            let dst2 = src1.lerp(dst1, (mlen - 20.) / mlen);
            path.quadratic_to(src1, src2);
            path.line_to(dst2);
            path.quadratic_to(dst1, dst);
        } else {
            let mid = src1.lerp(dst1, 0.5);
            path.quadratic_to(src1, mid);
            path.quadratic_to(dst1, dst);
        }
        let bounds = path.bounds();

        node.left = px(bounds.min.x);
        node.top = px(bounds.min.y);
        node.width = px(bounds.width());
        node.height = px(bounds.height());
        node.right = Val::Auto;
        node.bottom = Val::Auto;
        node.position_type = PositionType::Absolute;

        match material_node {
            Some(material_handle) => {
                let material = r_materials.get_mut(material_handle).unwrap();
                material.update(&path, &mut r_bindings);
            }
            None => {
                let buffer = r_bindings.add(ShaderStorageBuffer::default());
                let mut material = DrawPathMaterial::new(buffer);
                material.update(&path, &mut r_bindings);
                commands
                    .entity(insert.entity)
                    .insert(MaterialNode(r_materials.add(material)));
            }
        };

        // Update children for picking
        if children.is_none() {
            commands.entity(insert.entity).with_children(|parent| {
                parent
                    .spawn((
                        ConnectionHitBox::Src,
                        Node {
                            position_type: PositionType::Absolute,
                            width: px(24),
                            height: px(16),
                            left: px(src.x - bounds.min.x),
                            top: px(src.y - bounds.min.y - 8.0),
                            ..Default::default()
                        },
                        EntityCursor::System(bevy::window::SystemCursorIcon::Move),
                        Pickable {
                            is_hoverable: true,
                            should_block_lower: true,
                        },
                    ))
                    .observe(on_connection_drag_start)
                    .observe(on_terminal_drag)
                    .observe(on_terminal_drag_end)
                    .observe(on_terminal_drag_cancel);

                parent
                    .spawn((
                        ConnectionHitBox::Dst,
                        Node {
                            position_type: PositionType::Absolute,
                            width: px(24),
                            height: px(16),
                            left: px(dst.x - bounds.min.x - 24.0),
                            top: px(dst.y - bounds.min.y - 8.0),
                            ..Default::default()
                        },
                        EntityCursor::System(bevy::window::SystemCursorIcon::Move),
                        Pickable {
                            is_hoverable: true,
                            should_block_lower: true,
                        },
                    ))
                    .observe(on_connection_drag_start)
                    .observe(on_terminal_drag)
                    .observe(on_terminal_drag_end)
                    .observe(on_terminal_drag_cancel);
            });
        } else if let Some(children) = children {
            for &child in children {
                if let Ok((hit, mut node)) = q_connection_hit.get_mut(child) {
                    match hit {
                        ConnectionHitBox::Src => {
                            node.left = px(src.x - bounds.min.x);
                            node.top = px(src.y - bounds.min.y - 8.0);
                        }
                        ConnectionHitBox::Dst => {
                            node.left = px(dst.x - bounds.min.x - 24.0);
                            node.top = px(dst.y - bounds.min.y - 8.0);
                        }
                    }
                }
            }
        }
    }
}

#[allow(clippy::type_complexity)]
pub(crate) fn update_connection_shader(
    mut q_node: Query<
        (&ComputedUiRenderTargetInfo, &MaterialNode<DrawPathMaterial>),
        Or<(Changed<Connection>, Changed<ComputedNode>)>,
    >,
    mut r_materials: ResMut<Assets<DrawPathMaterial>>,
) {
    for (render_target, material_node) in q_node.iter_mut() {
        let material = r_materials.get_mut(material_node).unwrap();
        material.scale = 1.0 / render_target.scale_factor();
    }
}

fn on_graph_press(
    mut press: On<Pointer<Press>>,
    mut q_nodes: Query<&mut GraphNodeSelection, With<GraphNode>>,
    r_button: Res<ButtonInput<KeyCode>>,
) {
    press.propagate(false);
    let extend = r_button.pressed(KeyCode::ShiftLeft) || r_button.pressed(KeyCode::ShiftRight);
    if !extend {
        for mut selection in q_nodes.iter_mut() {
            if selection.selected {
                selection.selected = false;
            }
        }
    }
}

fn on_graph_drag_start(
    mut drag_start: On<Pointer<DragStart>>,
    q_children: Query<&Children>,
    mut q_selection: Query<(&mut Node, &mut Visibility), With<SelectionRect>>,
    q_graph_doc: Query<(), With<GraphDocument>>,
    mut r_gesture: ResMut<GestureState>,
) {
    drag_start.propagate(false);
    r_gesture.selection_anchor = drag_start.pointer_location.position;

    // Find the document
    let Some(&doc_ent) = q_children
        .get(drag_start.entity)
        .ok()
        .and_then(|children| children.iter().find(|&&ent| q_graph_doc.contains(ent)))
    else {
        return;
    };

    if let Ok(children) = q_children.get(doc_ent) {
        for child in children.iter() {
            if let Ok((mut rect_node, mut rect_vis)) = q_selection.get_mut(*child) {
                *rect_vis = Visibility::Visible;
                rect_node.left = px(r_gesture.selection_anchor.x);
                rect_node.top = px(r_gesture.selection_anchor.y);
                rect_node.width = px(0);
                rect_node.height = px(0);
            }
        }
    }
}

#[allow(clippy::type_complexity)]
fn on_graph_drag(
    mut drag: On<Pointer<Drag>>,
    q_children: Query<&Children>,
    mut q_selection: Query<&mut Node, With<SelectionRect>>,
    q_graph_doc: Query<(), With<GraphDocument>>,
    mut q_nodes: Query<
        (&Node, &ComputedNode, &ChildOf, &mut GraphNodeSelection),
        (With<GraphNode>, Without<SelectionRect>),
    >,
    r_gesture: ResMut<GestureState>,
) {
    drag.propagate(false);
    let graph_ent = drag.entity;
    let rect = Rect::from_corners(r_gesture.selection_anchor, drag.pointer_location.position);

    // Find the document
    let Some(&doc_ent) = q_children
        .get(graph_ent)
        .ok()
        .and_then(|children| children.iter().find(|&&ent| q_graph_doc.contains(ent)))
    else {
        return;
    };

    // Update the selection rect position
    if let Ok(children) = q_children.get(doc_ent) {
        for child in children.iter() {
            if let Ok(mut rect_node) = q_selection.get_mut(*child) {
                rect_node.left = px(rect.min.x);
                rect_node.top = px(rect.min.y);
                rect_node.width = px(rect.width());
                rect_node.height = px(rect.height());
            }
        }
    }

    for (node, computed_node, parent, mut selection) in q_nodes.iter_mut() {
        let pending = parent.0 == doc_ent
            && match (node.left, node.top) {
                (Val::Px(x), Val::Px(y)) => {
                    let node_rect = Rect::from_center_size(
                        Vec2::new(x, y),
                        computed_node.size() * computed_node.inverse_scale_factor(),
                    );
                    rect_overlaps(&rect, &node_rect)
                }
                _ => {
                    warn_once!("Graph node position should be in pixels");
                    false
                }
            };
        if selection.pending != pending {
            selection.pending = pending;
        }
    }
}

fn rect_overlaps(a: &Rect, b: &Rect) -> bool {
    !(a.max.x < b.min.x || a.min.x > b.max.x || a.max.y < b.min.y || a.min.y > b.max.y)
}

fn on_graph_drag_end(
    mut drag_end: On<Pointer<DragEnd>>,
    q_children: Query<&Children>,
    q_graph_doc: Query<(), With<GraphDocument>>,
    mut q_selection: Query<&mut Visibility, With<SelectionRect>>,
    mut q_nodes: Query<&mut GraphNodeSelection, With<GraphNode>>,
) {
    drag_end.propagate(false);

    // Find the document
    let Some(&doc_ent) = q_children
        .get(drag_end.entity)
        .ok()
        .and_then(|children| children.iter().find(|&&ent| q_graph_doc.contains(ent)))
    else {
        return;
    };

    // Hide the selection rect
    if let Ok(children) = q_children.get(doc_ent) {
        for child in children.iter() {
            if let Ok(mut rect_vis) = q_selection.get_mut(*child) {
                *rect_vis = Visibility::Hidden;
            }
        }
    }

    // Finalize the selection bits.
    for mut selection in q_nodes.iter_mut() {
        if selection.pending {
            selection.selected = true;
            selection.pending = false;
        }
    }
}

fn on_graph_drag_cancel(
    mut cancel: On<Pointer<Cancel>>,
    q_children: Query<&Children>,
    q_graph_doc: Query<(), With<GraphDocument>>,
    mut q_selection: Query<&mut Visibility, With<SelectionRect>>,
    mut q_nodes: Query<&mut GraphNodeSelection, With<GraphNode>>,
) {
    cancel.propagate(false);

    // Find the document
    let Some(&doc_ent) = q_children
        .get(cancel.entity)
        .ok()
        .and_then(|children| children.iter().find(|&&ent| q_graph_doc.contains(ent)))
    else {
        return;
    };

    // Hide the selection rect
    if let Ok(children) = q_children.get(doc_ent) {
        for child in children.iter() {
            if let Ok(mut rect_vis) = q_selection.get_mut(*child) {
                *rect_vis = Visibility::Hidden;
            }
        }
    }

    // Clear the selection bits
    for mut selection in q_nodes.iter_mut() {
        if selection.pending {
            selection.pending = false;
        }
    }
}

fn on_graph_scroll(
    mut scroll: On<Pointer<Scroll>>,
    q_graph: Query<(&ComputedNode, &GraphBounds, &Children), With<Graph>>,
    mut q_doc: Query<&mut Node, (With<GraphDocument>, Without<Graph>)>,
) {
    if let Ok((computed_node, &GraphBounds(bounds), children)) = q_graph.get(scroll.entity) {
        scroll.propagate(false);

        // Find the document
        let Some(doc_ent) = children.iter().find(|&&ent| q_doc.contains(ent)) else {
            return;
        };
        let Ok(mut doc_node) = q_doc.get_mut(*doc_ent) else {
            return;
        };

        let visible_size = computed_node.size() * computed_node.inverse_scale_factor;

        let scroll_limits = Rect {
            min: bounds.min,
            max: (bounds.max - visible_size).max(bounds.min),
        };

        let scroll_pos = Vec2::new(
            match doc_node.left {
                Val::Px(px) => -px,
                _ => 0.0,
            },
            match doc_node.top {
                Val::Px(px) => -px,
                _ => 0.0,
            },
        );

        let scroll_delta = Vec2::new(scroll.x, scroll.y)
            * match scroll.unit {
                MouseScrollUnit::Line => 14.0, // Guess for now. No idea how we'd get the real value.
                MouseScrollUnit::Pixel => 1.0,
            };

        let next_scroll_pos =
            (scroll_pos - scroll_delta).clamp(scroll_limits.min, scroll_limits.max);

        if next_scroll_pos != scroll_pos {
            doc_node.left = px(-next_scroll_pos.x);
            doc_node.top = px(-next_scroll_pos.y);
        }
    }
}

fn on_graph_node_press(
    mut press: On<Pointer<Press>>,
    mut q_nodes: Query<(Entity, &mut GraphNodeSelection), With<GraphNode>>,
    r_button: Res<ButtonInput<KeyCode>>,
) {
    press.propagate(false);
    let extend = r_button.pressed(KeyCode::ShiftLeft) || r_button.pressed(KeyCode::ShiftRight);
    let toggle = r_button.pressed(KeyCode::ControlLeft)
        || r_button.pressed(KeyCode::ControlLeft)
        || r_button.pressed(KeyCode::SuperLeft)
        || r_button.pressed(KeyCode::SuperRight)
        || r_button.pressed(KeyCode::Meta);
    if let Ok((_, mut selection)) = q_nodes.get_mut(press.entity) {
        if extend {
            // Selected pressed node without deselecting others
            if !selection.selected {
                selection.selected = true;
            }
        } else if toggle {
            // Flip the selection state of the pressed entity
            selection.selected = !selection.selected;
        } else {
            // Clear selection for all nodes except pressed one, unless pressed one is already
            // selected
            if !selection.selected {
                selection.selected = true;
                for (node, mut selection) in q_nodes.iter_mut() {
                    if selection.selected && node != press.entity {
                        selection.selected = false;
                    }
                }
            }
        }
    }
}

fn on_graph_node_drag_start(
    mut drag_start: On<Pointer<DragStart>>,
    mut q_node: Query<
        (&ChildOf, &Node, &GraphNodeSelection, &mut GraphNodeOffset),
        With<GraphNode>,
    >,
    q_graph_doc: Query<&ChildOf, (With<GraphDocument>, Without<Graph>)>,
    q_graph: Query<(), With<Graph>>,
    mut r_gesture: ResMut<GestureState>,
    mut commands: Commands,
) {
    drag_start.propagate(false);
    if let Ok((&ChildOf(parent), _, _, _)) = q_node.get(drag_start.entity) {
        if let Ok(&ChildOf(grandparent)) = q_graph_doc.get(parent) {
            if q_graph.contains(grandparent) {
                r_gesture.graph = Some(grandparent);
                r_gesture.gesture = Gesture::Move;

                for (_, node, selection, mut node_offset) in q_node.iter_mut() {
                    if selection.selected {
                        match (node.left, node.top) {
                            (Val::Px(x), Val::Px(y)) => {
                                node_offset.0.x = x;
                                node_offset.0.y = y;
                            }
                            _ => warn_once!("Graph node position should be in pixels"),
                        }
                    }
                }

                commands.trigger(MoveNodesEvent {
                    graph: parent,
                    distance: Vec2::ZERO,
                });
            } else {
                warn_once!(
                    "Graph content {} has a parent, but it is not a graph",
                    drag_start.entity
                );
            }
        } else {
            warn_once!(
                "Graph node {} has a parent, but it is not a GraphContent",
                drag_start.entity
            );
        }
    } else {
        warn_once!("Graph node {} has no parent", drag_start.entity);
    }
}

fn on_graph_node_drag(
    mut drag: On<Pointer<Drag>>,
    r_gesture: ResMut<GestureState>,
    mut commands: Commands,
) {
    drag.propagate(false);
    if matches!(r_gesture.gesture, Gesture::Move) {
        commands.trigger(MoveNodesEvent {
            graph: r_gesture.graph.unwrap(),
            distance: drag.distance,
        });
    }
}

fn on_graph_node_drag_end(mut drag_end: On<Pointer<DragEnd>>, mut r_gesture: ResMut<GestureState>) {
    drag_end.propagate(false);
    if matches!(r_gesture.gesture, Gesture::Move) {
        r_gesture.graph = None;
        r_gesture.gesture = Gesture::Idle;
    }
}

// Not used because BSN limits
fn on_graph_node_drag_cancel(mut cancel: On<Pointer<Cancel>>, mut r_gesture: ResMut<GestureState>) {
    cancel.propagate(false);
    if matches!(r_gesture.gesture, Gesture::Move) {
        r_gesture.graph = None;
        r_gesture.gesture = Gesture::Idle;
    }
}

#[allow(clippy::type_complexity)]
fn on_terminal_drag_start(
    mut drag_start: On<Pointer<DragStart>>,
    q_parent: Query<&ChildOf>,
    mut q_terminal: Query<&Terminal>,
    q_graph: Query<&Children, With<Graph>>,
    q_graph_doc: Query<(Entity, &UiGlobalTransform, &ComputedNode), With<GraphDocument>>,
    mut r_gesture: ResMut<GestureState>,
    mut commands: Commands,
) {
    drag_start.propagate(false);
    if let Ok(terminal) = q_terminal.get_mut(drag_start.entity) {
        // Get graph
        let Some(graph) = q_parent
            .iter_ancestors(drag_start.entity)
            .find(|e| q_graph.contains(*e))
        else {
            return;
        };

        // Find the document
        let Some((connections, container_transform, container_computed_node)) = q_graph
            .get(graph)
            .ok()
            .and_then(|children| children.iter().find_map(|&ent| q_graph_doc.get(ent).ok()))
        else {
            return;
        };

        let anchor = if *terminal == Terminal::Input {
            ConnectionAnchor::InputTerminal(drag_start.entity)
        } else {
            ConnectionAnchor::OutputTerminal(drag_start.entity)
        };
        let target = ConnectionTarget::Location(
            drag_start.pointer_location.position
                - (container_transform.translation - container_computed_node.size() * 0.5)
                    * container_computed_node.inverse_scale_factor(),
        );

        r_gesture.graph = Some(graph);
        r_gesture.gesture = Gesture::Connect {
            connections,
            anchor,
            target,
        };
        commands
            .entity(graph)
            .insert(EntityCursor::System(bevy::window::SystemCursorIcon::Copy));
        commands.trigger(ConnectEvent::from_gesture(
            graph,
            connections,
            anchor,
            target,
            DragAction::StartNew,
        ));
    }
}

fn on_terminal_drag(
    mut drag: On<Pointer<Drag>>,
    mut r_gesture: ResMut<GestureState>,
    q_graph_doc: Query<(&UiGlobalTransform, &ComputedNode), With<GraphDocument>>,
    mut commands: Commands,
) {
    drag.propagate(false);
    if let Some(graph) = r_gesture.graph {
        debug!("on_terminal_drag");
        let connection_target = r_gesture.connection_target;
        if let Gesture::Connect {
            anchor,
            connections: container,
            ref mut target,
        } = r_gesture.gesture
        {
            let Ok((container_transform, container_computed_node)) = q_graph_doc.get(container)
            else {
                return;
            };

            *target = connection_target.unwrap_or(ConnectionTarget::Location(
                drag.pointer_location.position
                    - (container_transform.translation - container_computed_node.size() * 0.5)
                        * container_computed_node.inverse_scale_factor(),
            ));
            commands.trigger(ConnectEvent::from_gesture(
                graph,
                container,
                anchor,
                *target,
                DragAction::InProgress,
            ));
        }
    }
}

fn on_terminal_drag_end(
    mut drag_end: On<Pointer<DragEnd>>,
    mut r_gesture: ResMut<GestureState>,
    mut commands: Commands,
) {
    drag_end.propagate(false);
    if let Some(graph) = r_gesture.graph {
        if let Gesture::Connect {
            anchor,
            connections: container,
            target,
        } = r_gesture.gesture
        {
            commands.trigger(ConnectEvent::from_gesture(
                graph,
                container,
                anchor,
                target,
                DragAction::Finish,
            ));
        }
        commands.entity(graph).insert(EntityCursor::System(
            bevy::window::SystemCursorIcon::Crosshair,
        ));
    }
    r_gesture.graph = None;
    r_gesture.gesture = Gesture::Idle;
}

fn on_terminal_drag_cancel(
    mut cancel: On<Pointer<Cancel>>,
    mut r_gesture: ResMut<GestureState>,
    mut commands: Commands,
) {
    cancel.propagate(false);
    if let Some(graph) = r_gesture.graph {
        if let Gesture::Connect {
            anchor,
            connections: container,
            target,
        } = r_gesture.gesture
        {
            commands.trigger(ConnectEvent::from_gesture(
                graph,
                container,
                anchor,
                target,
                DragAction::Cancel,
            ));
        }
        commands.entity(graph).insert(EntityCursor::System(
            bevy::window::SystemCursorIcon::Crosshair,
        ));
    }
    r_gesture.graph = None;
    r_gesture.gesture = Gesture::Idle;
}

#[allow(clippy::type_complexity)]
fn on_terminal_drag_enter(
    mut drag_enter: On<Pointer<DragEnter>>,
    mut q_terminal: Query<&Terminal>,
    mut r_gesture: ResMut<GestureState>,
    mut commands: Commands,
) {
    drag_enter.propagate(false);
    if let Some(graph) = r_gesture.graph {
        let Ok(terminal) = q_terminal.get_mut(drag_enter.entity) else {
            return;
        };
        if let Gesture::Connect {
            anchor,
            connections: container,
            ref mut target,
        } = r_gesture.gesture
        {
            let new_target = if *terminal == Terminal::Input {
                ConnectionTarget::InputTerminal(drag_enter.entity)
            } else {
                ConnectionTarget::OutputTerminal(drag_enter.entity)
            };

            // Must only connect inputs to outputs
            let is_valid = matches!(
                (anchor, new_target),
                (
                    ConnectionAnchor::InputTerminal(_),
                    ConnectionTarget::OutputTerminal(_)
                ) | (
                    ConnectionAnchor::OutputTerminal(_),
                    ConnectionTarget::InputTerminal(_)
                ) | (
                    ConnectionAnchor::EdgeStart(_),
                    ConnectionTarget::OutputTerminal(_)
                ) | (
                    ConnectionAnchor::EdgeEnd(_),
                    ConnectionTarget::InputTerminal(_)
                )
            );

            if is_valid {
                *target = new_target;
                r_gesture.connection_target = Some(new_target);
                commands.trigger(ConnectEvent::from_gesture(
                    graph,
                    container,
                    anchor,
                    new_target,
                    DragAction::InProgress,
                ));
            }
        }
    }
}

fn on_terminal_drag_leave(
    mut drag_end: On<Pointer<DragLeave>>,
    mut r_gesture: ResMut<GestureState>,
) {
    drag_end.propagate(false);
    if let Gesture::Connect {
        anchor: _,
        connections: _,
        target: _,
    } = r_gesture.gesture
    {
        r_gesture.connection_target = None;
    }
}

fn on_terminal_drop(
    mut drag_end: On<Pointer<DragDrop>>,
    mut r_gesture: ResMut<GestureState>,
    mut commands: Commands,
) {
    drag_end.propagate(false);
    if let Some(graph) = r_gesture.graph {
        if let Gesture::Connect {
            anchor,
            connections: container,
            target,
        } = r_gesture.gesture
        {
            commands.trigger(ConnectEvent::from_gesture(
                graph,
                container,
                anchor,
                target,
                DragAction::Finish,
            ));
        }
        commands.entity(graph).insert(EntityCursor::System(
            bevy::window::SystemCursorIcon::Crosshair,
        ));
        r_gesture.graph = None;
        r_gesture.connection_target = None;
        r_gesture.gesture = Gesture::Idle;
    }
}

#[allow(clippy::type_complexity)]
fn on_connection_drag_start(
    mut drag_start: On<Pointer<DragStart>>,
    q_parent: Query<&ChildOf>,
    mut q_connection_hitbox: Query<&ConnectionHitBox>,
    q_connection: Query<&Connection>,
    q_graph_doc: Query<(Entity, &UiGlobalTransform, &ComputedNode), With<GraphDocument>>,
    mut r_gesture: ResMut<GestureState>,
    mut commands: Commands,
) {
    drag_start.propagate(false);
    if let Ok(hitbox) = q_connection_hitbox.get_mut(drag_start.entity) {
        // Get connection
        let Ok(&ChildOf(conn_id)) = q_parent.get(drag_start.entity) else {
            warn_once!("Connection hitbox should be a child of Connection");
            return;
        };
        let Ok(connection) = q_connection.get(conn_id) else {
            warn_once!("Connection hitbox should be a child of Connection");
            return;
        };

        // Get document
        let Some(doc) = q_parent
            .iter_ancestors(drag_start.entity)
            .find(|e| q_graph_doc.contains(*e))
        else {
            warn_once!("Connection hitbox should be contained within a graph document");
            return;
        };

        // Get graph
        let Ok(&ChildOf(graph)) = q_parent.get(doc) else {
            warn_once!("Graph documet should be contained within a graph");
            return;
        };

        // If we hit the Src, then Dst is the anchor, and vice-versa.
        let (target, anchor) = match hitbox {
            ConnectionHitBox::Src => (
                match connection.src {
                    ConnectionTerminus::Terminal(output) => {
                        ConnectionTarget::OutputTerminal(output)
                    }
                    ConnectionTerminus::Location(pos) => ConnectionTarget::Location(pos),
                },
                match connection.dst {
                    ConnectionTerminus::Terminal(input) => ConnectionAnchor::InputTerminal(input),
                    ConnectionTerminus::Location(_) => unreachable!("Should be already connected"),
                },
            ),
            ConnectionHitBox::Dst => (
                match connection.dst {
                    ConnectionTerminus::Terminal(input) => ConnectionTarget::InputTerminal(input),
                    ConnectionTerminus::Location(pos) => ConnectionTarget::Location(pos),
                },
                match connection.src {
                    ConnectionTerminus::Terminal(output) => {
                        ConnectionAnchor::OutputTerminal(output)
                    }
                    ConnectionTerminus::Location(_) => unreachable!("Should be already connected"),
                },
            ),
        };

        r_gesture.graph = Some(graph);
        r_gesture.connection_target = None;
        r_gesture.gesture = Gesture::Connect {
            connections: doc,
            anchor,
            target,
        };
        //     commands
        //         .entity(graph)
        //         .insert(EntityCursor::System(bevy::window::SystemCursorIcon::Copy));
        commands.trigger(ConnectEvent::from_gesture(
            graph,
            doc,
            anchor,
            target,
            DragAction::StartEdit(conn_id),
        ));
    }
}

/// Change node outline appearance based on selection and hover
#[allow(clippy::type_complexity)]
pub(crate) fn update_node_outlines(
    q_node: Query<
        (Entity, &GraphNodeSelection, &Hovered),
        (
            With<GraphNode>,
            Or<(Changed<GraphNodeSelection>, Changed<Hovered>)>,
        ),
    >,
    mut commands: Commands,
) {
    for (node, selection, &Hovered(hovered)) in q_node.iter() {
        if selection.show_selection() {
            commands.entity(node).insert(Outline {
                width: px(2),
                offset: px(1),
                color: palette::WHITE.with_alpha(0.2),
            });
        } else if hovered {
            commands.entity(node).insert(Outline {
                width: px(2),
                offset: px(1),
                color: palette::WHITE.with_alpha(0.1),
            });
        } else {
            commands.entity(node).remove::<Outline>();
        }
    }
}

/// For each connection that is attached to a node whose transform has changed, force
/// a mutation on the connection to trigger a re-layout.
pub(crate) fn update_terminal_positions(
    q_connections: Query<(Entity, &Connection)>,
    q_terminals: Query<&UiGlobalTransform, (With<Terminal>, Changed<UiGlobalTransform>)>,
    mut commands: Commands,
) {
    for (connection_ent, connection) in q_connections {
        let src_changed = match connection.src {
            ConnectionTerminus::Terminal(term) => q_terminals.contains(term),
            ConnectionTerminus::Location(_) => false,
        };

        let dst_changed = match connection.dst {
            ConnectionTerminus::Terminal(term) => q_terminals.contains(term),
            ConnectionTerminus::Location(_) => false,
        };

        if src_changed || dst_changed {
            // Re-insert connection to force Insert observer update.
            commands.entity(connection_ent).insert(connection.clone());
        }
    }
}

#[allow(clippy::type_complexity)]
pub(crate) fn update_graph_bounds(
    mut q_graph: Query<(&Children, &ComputedNode, &mut GraphBounds), With<Graph>>,
    q_doc: Query<
        (&Node, &Children, &UiGlobalTransform, &ComputedNode),
        (With<GraphDocument>, Without<Graph>),
    >,
    q_nodes: Query<(&UiGlobalTransform, &ComputedNode), (With<GraphNode>, Without<Graph>)>,
) {
    for (children, graph_computed_node, mut graph_bounds) in q_graph.iter_mut() {
        let mut new_bounds = Rect::EMPTY;

        // Find doc element
        let Some((doc_node, doc_children, doc_transform, doc_computed_node)) =
            children.iter().find_map(|&ent| q_doc.get(ent).ok())
        else {
            continue;
        };

        // Compute bounds of each node relative to document entity and add to bounds.
        let doc_origin = (doc_transform.translation - doc_computed_node.size() * 0.5)
            * doc_computed_node.inverse_scale_factor();
        for child_id in doc_children {
            if let Ok((node_transform, computed_node)) = q_nodes.get(*child_id) {
                let child_pos =
                    node_transform.translation * computed_node.inverse_scale_factor() - doc_origin;
                new_bounds = new_bounds.union(
                    Rect::from_center_size(
                        child_pos,
                        computed_node.size() * computed_node.inverse_scale_factor(),
                    )
                    .inflate(50.0),
                );
            }
        }

        let viewport_size = graph_computed_node.size() * graph_computed_node.inverse_scale_factor();
        let scroll_pos = Vec2::new(
            match doc_node.left {
                Val::Px(px) => px,
                _ => 0.0,
            },
            match doc_node.top {
                Val::Px(px) => px,
                _ => 0.0,
            },
        );

        let visible_area = Rect::from_corners(-scroll_pos, viewport_size - scroll_pos);
        new_bounds = new_bounds.union(visible_area);

        if graph_bounds.0 != new_bounds {
            graph_bounds.0 = new_bounds;
        }
    }
}
