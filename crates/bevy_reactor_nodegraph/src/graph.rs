use std::ops::Mul;

use bevy::{
    asset::Assets,
    color::{Alpha, Color},
    ecs::{
        component::Component,
        entity::Entity,
        hierarchy::ChildOf,
        lifecycle::Insert,
        observer::On,
        query::{Changed, Or, With},
        system::{Commands, Query, ResMut},
    },
    feathers::{constants::fonts, cursor::EntityCursor, font_styles::InheritableFont, palette},
    log::{info, warn_once},
    math::Vec2,
    picking::{
        events::{Drag, DragEnd, DragStart, Pointer, Press},
        hover::Hovered,
    },
    render::storage::ShaderStorageBuffer,
    scene2::{Scene, bsn, on},
    ui::{
        AlignItems, AlignSelf, BackgroundColor, BorderColor, BorderRadius, BoxShadow, ComputedNode,
        ComputedUiRenderTargetInfo, Display, FlexDirection, JustifyContent, Node, Outline,
        PositionType, UiRect, UiTransform, Val, Val2, px,
    },
    ui_render::prelude::MaterialNode,
};

use crate::{DrawPathMaterial, DrawablePath, GestureState, MoveNodesEvent};

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

pub fn display_graph() -> impl Scene {
    bsn! {
        Graph
        on(on_drag_graph_press)
        on(on_drag_graph_start)
        on(on_drag_graph)
        on(on_drag_graph_end)
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
        on(on_drag_graph_node_press)
        on(on_drag_graph_node_start)
        on(on_drag_graph_node)
        on(on_drag_graph_node_end)
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
                width: px(6),
                height: px(6),
                left: px(-11),
            }
            InputTerminal
            BorderRadius::MAX
            BackgroundColor(color)
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
                width: px(6),
                height: px(6),
                right: px(-11),
            }
            OutputTerminal
            BorderRadius::MAX
            BackgroundColor(color)
        ]
    }
}

/// An input terminal where connections can attach to.
#[derive(Component, Clone, Default)]
pub struct InputTerminal;

/// An input terminal where connections can attach to.
#[derive(Component, Clone, Default)]
pub struct OutputTerminal;

/// Displays a stroked path between two nodes.
#[derive(Component, Clone, Default)]
#[component(immutable)]
#[require(Node)]
pub struct GraphEdge {
    /// Pixel position of the source terminal.
    pub src_pos: Vec2,

    /// Pixel position of the destination terminal.
    pub dst_pos: Vec2,

    /// Color of this edge
    pub color: Color,
}

pub(crate) fn on_insert_edge(
    insert: On<Insert, GraphEdge>,
    mut q_node: Query<(
        &GraphEdge,
        &mut Node,
        Option<&MaterialNode<DrawPathMaterial>>,
    )>,
    mut r_materials: ResMut<Assets<DrawPathMaterial>>,
    mut r_bindings: ResMut<Assets<ShaderStorageBuffer>>,
    mut commands: Commands,
) {
    // Adjust node
    if let Ok((edge, mut node, material_node)) = q_node.get_mut(insert.entity) {
        let mut path = DrawablePath::new(edge.color.into(), 1.5);
        let src = edge.src_pos;
        let dst = edge.dst_pos;
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
        Or<(Changed<GraphEdge>, Changed<ComputedNode>)>,
    >,
    mut r_materials: ResMut<Assets<DrawPathMaterial>>,
) {
    for (render_target, material_node) in q_node.iter_mut() {
        let material = r_materials.get_mut(material_node).unwrap();
        material.scale = 1.0 / render_target.scale_factor();
    }
}

fn on_drag_graph_press(
    mut press: On<Pointer<Press>>,
    mut q_nodes: Query<&mut GraphNodeSelection, With<GraphNode>>,
) {
    press.propagate(false);
    for mut selection in q_nodes.iter_mut() {
        if selection.selected {
            selection.selected = false;
        }
    }
}

fn on_drag_graph_start(mut drag_start: On<Pointer<DragStart>>) {
    drag_start.propagate(false);
    info!("Drag graph");
}

fn on_drag_graph(drag: On<Pointer<Drag>>) {}

fn on_drag_graph_end(drag_end: On<Pointer<DragEnd>>) {}

fn on_drag_graph_node_press(
    mut press: On<Pointer<Press>>,
    mut q_nodes: Query<(Entity, &mut GraphNodeSelection), With<GraphNode>>,
) {
    press.propagate(false);
    for (node, mut selection) in q_nodes.iter_mut() {
        let should_select = node == press.entity;
        if selection.selected != should_select {
            selection.selected = should_select;
        }
    }
}

fn on_drag_graph_node_start(
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
            r_gesture.node = Some(drag_start.entity);

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

fn on_drag_graph_node(
    mut drag: On<Pointer<Drag>>,
    r_gesture: ResMut<GestureState>,
    mut commands: Commands,
) {
    drag.propagate(false);
    if r_gesture.node == Some(drag.entity) {
        commands.trigger(MoveNodesEvent {
            graph: r_gesture.graph.unwrap(),
            distance: drag.distance,
        });
    }
}

fn on_drag_graph_node_end(mut drag_end: On<Pointer<DragEnd>>, mut r_gesture: ResMut<GestureState>) {
    drag_end.propagate(false);
    if r_gesture.node == Some(drag_end.entity) {
        r_gesture.graph = None;
        r_gesture.node = None;
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
