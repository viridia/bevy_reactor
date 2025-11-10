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
    input::{ButtonInput, keyboard::KeyCode},
    log::warn_once,
    math::{Rect, Vec2},
    picking::{
        Pickable,
        events::{
            Cancel, Drag, DragDrop, DragEnd, DragEnter, DragLeave, DragStart, Pointer, Press,
        },
        hover::Hovered,
    },
    render::storage::ShaderStorageBuffer,
    scene2::{Scene, bsn, on, template_value},
    ui::{
        AlignItems, AlignSelf, BackgroundColor, BorderColor, BorderRadius, BoxShadow, ComputedNode,
        ComputedUiRenderTargetInfo, Display, FlexDirection, JustifyContent, Node, Outline,
        PositionType, UiGlobalTransform, UiRect, UiTransform, Val, Val2, px, widget::Text,
    },
    ui_render::prelude::MaterialNode,
};

use crate::{
    ConnectEvent, ConnectionAnchor, ConnectionTarget, DragAction, DrawPathMaterial, DrawablePath,
    Gesture, GestureState, MoveNodesEvent,
};

/// A node graph.
#[derive(Component, Clone, Default)]
pub struct Graph;

/// A node within a node graph.
#[derive(Component, Clone, Default)]
#[require(GraphNodeSelection)]
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

pub fn display_graph() -> impl Scene {
    bsn! {
        Graph
        EntityCursor::System(bevy::window::SystemCursorIcon::Crosshair)
        on(on_graph_press)
        on(on_graph_drag_start)
        on(on_graph_drag)
        on(on_graph_drag_end)
        on(on_graph_drag_cancel)
        [
            :display_selection_rect()
            Node {
                width: px(100),
                height: px(100),
            }
        ]
    }
}

pub fn display_graph_node(position: Vec2) -> impl Scene {
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
        GraphNodeOffset
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
        // on(on_graph_node_drag_cancel)
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

pub fn display_graph_node_title() -> impl Scene {
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

pub fn display_graph_node_body() -> impl Scene {
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

pub fn display_selection_rect() -> impl Scene {
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
    /// An input terminal that connects to an edge dst
    #[default]
    Input,
    /// An output terminal that connects to an edge src
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
#[require(Node)]
pub struct Connection {
    /// Pixel position of the source terminal.
    pub src: ConnectionTerminus,

    /// Pixel position of the destination terminal.
    pub dst: ConnectionTerminus,

    /// Color of this edge
    pub color: Color,
}

pub(crate) fn on_insert_edge(
    insert: On<Insert, Connection>,
    mut q_node: Query<(
        &Connection,
        &mut Node,
        Option<&MaterialNode<DrawPathMaterial>>,
    )>,
    q_terminals: Query<(&UiGlobalTransform, &ComputedNode)>,
    mut r_materials: ResMut<Assets<DrawPathMaterial>>,
    mut r_bindings: ResMut<Assets<ShaderStorageBuffer>>,
    mut commands: Commands,
) {
    // Adjust node
    if let Ok((edge, mut node, material_node)) = q_node.get_mut(insert.entity) {
        let mut path = DrawablePath::new(edge.color.into(), 1.5);
        let src = match edge.src {
            ConnectionTerminus::Terminal(entity) => {
                if let Ok((transform, computed_node)) = q_terminals.get(entity) {
                    transform.translation * computed_node.inverse_scale_factor()
                } else {
                    Vec2::ZERO
                }
            }
            ConnectionTerminus::Location(pos) => pos,
        };
        let dst = match edge.dst {
            ConnectionTerminus::Terminal(entity) => {
                if let Ok((transform, computed_node)) = q_terminals.get(entity) {
                    transform.translation * computed_node.inverse_scale_factor()
                } else {
                    Vec2::ZERO
                }
            }
            ConnectionTerminus::Location(pos) => pos,
        };
        let dx = (dst.x - src.x).abs().mul(0.3).min(20.);
        let src1 = src + Vec2::new(dx, 0.);
        let dst1 = dst - Vec2::new(dx, 0.);
        path.move_to(src);
        let mlen = src1.distance(dst1);
        if mlen > 40. {
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

        node.left = Val::Px(bounds.min.x);
        node.top = Val::Px(bounds.min.y);
        node.width = Val::Px(bounds.width());
        node.height = Val::Px(bounds.height());
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
    }
}

#[allow(clippy::type_complexity)]
pub(crate) fn update_edge_shader(
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
    mut r_gesture: ResMut<GestureState>,
) {
    drag_start.propagate(false);
    r_gesture.selection_anchor = drag_start.pointer_location.position;
    if let Ok(children) = q_children.get(drag_start.entity) {
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
    mut q_nodes: Query<
        (&Node, &ComputedNode, &ChildOf, &mut GraphNodeSelection),
        (With<GraphNode>, Without<SelectionRect>),
    >,
    r_gesture: ResMut<GestureState>,
) {
    drag.propagate(false);
    let graph_ent = drag.entity;
    let rect = Rect::from_corners(r_gesture.selection_anchor, drag.pointer_location.position);
    if let Ok(children) = q_children.get(drag.entity) {
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
        let pending = parent.0 == graph_ent
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
    mut q_selection: Query<&mut Visibility, With<SelectionRect>>,
    mut q_nodes: Query<&mut GraphNodeSelection, With<GraphNode>>,
) {
    drag_end.propagate(false);
    if let Ok(children) = q_children.get(drag_end.entity) {
        for child in children.iter() {
            if let Ok(mut rect_vis) = q_selection.get_mut(*child) {
                *rect_vis = Visibility::Hidden;
            }
        }
    }

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
    mut q_selection: Query<&mut Visibility, With<SelectionRect>>,
    mut q_nodes: Query<&mut GraphNodeSelection, With<GraphNode>>,
) {
    cancel.propagate(false);
    if let Ok(children) = q_children.get(cancel.entity) {
        for child in children.iter() {
            if let Ok(mut rect_vis) = q_selection.get_mut(*child) {
                *rect_vis = Visibility::Hidden;
            }
        }
    }

    for mut selection in q_nodes.iter_mut() {
        if selection.pending {
            selection.selected = true;
            selection.pending = false;
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
    q_graph: Query<(), With<Graph>>,
    mut r_gesture: ResMut<GestureState>,
    mut commands: Commands,
) {
    drag_start.propagate(false);
    if let Ok((&ChildOf(parent), _, _, _)) = q_node.get(drag_start.entity) {
        if q_graph.contains(parent) {
            r_gesture.graph = Some(parent);
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
                "Graph node {} has a parent, but it is not a graph",
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
    q_graph: Query<(), With<Graph>>,
    mut r_gesture: ResMut<GestureState>,
    mut commands: Commands,
) {
    // Get graph!
    drag_start.propagate(false);
    if let Ok(terminal) = q_terminal.get_mut(drag_start.entity) {
        let Some(graph) = q_parent
            .iter_ancestors(drag_start.entity)
            .find(|e| q_graph.contains(*e))
        else {
            return;
        };
        let anchor = if *terminal == Terminal::Input {
            ConnectionAnchor::InputTerminal(drag_start.entity)
        } else {
            ConnectionAnchor::OutputTerminal(drag_start.entity)
        };
        let target = ConnectionTarget::Location(drag_start.pointer_location.position);
        r_gesture.graph = Some(graph);
        r_gesture.gesture = Gesture::Connect { anchor, target };
        commands
            .entity(graph)
            .insert(EntityCursor::System(bevy::window::SystemCursorIcon::Copy));
        commands.trigger(ConnectEvent::from_gesture(
            graph,
            anchor,
            target,
            DragAction::Start,
        ));
    }
}

fn on_terminal_drag(
    mut drag: On<Pointer<Drag>>,
    mut r_gesture: ResMut<GestureState>,
    mut commands: Commands,
) {
    drag.propagate(false);
    if let Some(graph) = r_gesture.graph {
        let connection_target = r_gesture.connection_target;
        if let Gesture::Connect {
            anchor,
            ref mut target,
        } = r_gesture.gesture
        {
            *target = connection_target
                .unwrap_or(ConnectionTarget::Location(drag.pointer_location.position));
            commands.trigger(ConnectEvent::from_gesture(
                graph,
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
        if let Gesture::Connect { anchor, target } = r_gesture.gesture {
            commands.trigger(ConnectEvent::from_gesture(
                graph,
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
        if let Gesture::Connect { anchor, target } = r_gesture.gesture {
            commands.trigger(ConnectEvent::from_gesture(
                graph,
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
        if let Gesture::Connect { anchor, target } = r_gesture.gesture {
            commands.trigger(ConnectEvent::from_gesture(
                graph,
                anchor,
                target,
                DragAction::Finish,
            ));
        }
        commands.entity(graph).insert(EntityCursor::System(
            bevy::window::SystemCursorIcon::Crosshair,
        ));
        r_gesture.graph = None;
        r_gesture.gesture = Gesture::Idle;
    }
}

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
