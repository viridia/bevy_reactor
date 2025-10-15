use std::ops::Mul;

use bevy::{
    asset::Assets,
    color::Color,
    ecs::{
        component::Component,
        entity::Entity,
        lifecycle::Insert,
        observer::On,
        query::{Changed, Or},
        system::{Commands, Query, ResMut},
    },
    math::Vec2,
    picking::hover::Hovered,
    render::storage::ShaderStorageBuffer,
    scene2::{Scene, bsn},
    ui::{
        AlignItems, BorderColor, BorderRadius, ComputedNode, ComputedUiRenderTargetInfo, Display,
        FlexDirection, JustifyContent, Node, PositionType, UiRect, Val, px,
    },
    ui_render::prelude::MaterialNode,
};

use crate::{DrawPathMaterial, DrawablePath};

/// A node graph.
#[derive(Component, Default)]
pub struct Graph;

/// A node within a node graph.
#[derive(Component, Default)]
pub struct GraphNode {
    /// The coordinates of the node's upper-left corner.
    pub position: Vec2,
}

pub fn display_graph_node() -> impl Scene {
    bsn! {
        Node {
            display: Display::Flex,
            flex_direction: FlexDirection::Column,
            border: UiRect::all(Val::Px(5.0)),
            justify_content: JustifyContent::Start,
            align_items: AlignItems::Stretch,
        }
        // GraphNode
        Hovered::default()
        // TabIndex(0)
        BorderColor::all(Color::WHITE)
        BorderRadius::all(px(3))
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
            justify_content: JustifyContent::Center,
            align_items: AlignItems::Center,
            padding: UiRect::axes(px(4.0), px(2.0)),
        }
        GraphNodeTitle
        BorderColor::all(Color::WHITE)
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
            padding: UiRect::all(px(4.0)),
        }
        GraphNodeBody
    }
}

/// Marker that indicates whether this node is selected.
#[derive(Component, Default)]
pub struct GraphNodeSelected;

/// An input terminal where connections can attach to.
#[derive(Component, Default)]
pub struct InputTerminal {
    /// Color of the input terminal, which is typically used to indicate the data type of
    /// the connector.
    pub color: Color,

    /// The name of this input. This will be displayed unless the control widget has a label
    /// and is visible.
    pub label: String,

    /// Control to be rendered when input is not connected. Typically a slider, checkbox or
    /// color picker.
    pub control: Option<Entity>,
}

/// An input terminal where connections can attach to.
#[derive(Component, Default)]
pub struct OutputTerminal {
    /// Color of the input terminal, which is typically used to indicate the data type of
    /// the connector.
    pub color: Color,

    /// The name of this output.
    pub label: String,
}

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
