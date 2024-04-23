use bevy::{prelude::*, ui};
use bevy_reactor::*;

use crate::{colors, materials::DotGridMaterial};

use super::ScrollView;

fn style_node_graph(ss: &mut StyleBuilder) {
    ss.background_color(colors::U1);
}

fn style_node_graph_content(ss: &mut StyleBuilder) {
    ss.border(0)
        // .border_color(colors::X_RED)
        .min_width(ui::Val::Percent(100.))
        .min_height(ui::Val::Percent(100.));
}

fn style_node_graph_scroll(ss: &mut StyleBuilder) {
    ss.min_width(ui::Val::Px(2000.0));
}

/// An editable graph of nodes, connected by edges.
#[derive(Default)]
pub struct NodeGraph {
    /// Nodes within the node graph.
    pub children: ViewRef,

    /// Additional styles to be applied to the graph element.
    pub style: StyleHandle,
}

impl ViewTemplate for NodeGraph {
    fn create(&self, cx: &mut Cx) -> impl Into<ViewRef> {
        let mut ui_materials = cx
            .world_mut()
            .get_resource_mut::<Assets<DotGridMaterial>>()
            .unwrap();
        let material = ui_materials.add(DotGridMaterial {
            color_bg: LinearRgba::from(colors::U1).into(),
            color_fg: LinearRgba::from(colors::U3).into(),
        });

        ScrollView {
            children: Element::<MaterialNodeBundle<DotGridMaterial>>::new()
                .named("NodeGraph::Scroll")
                .insert(material.clone())
                .with_styles(style_node_graph_scroll)
                .with_children(self.children.clone())
                .into(),
            style: StyleHandle::new((style_node_graph, self.style.clone())),
            content_style: StyleHandle::new(style_node_graph_content),
            scroll_enable_x: true,
            scroll_enable_y: true,
            // ..default()
        }
    }
}

/// A node within a node graph.
#[derive(Default)]
pub struct NodeGraphNode {
    /// The coordinates of the node's upper-left corner.
    pub position: Signal<Vec2>,
    /// The title of the node.
    pub title: Signal<String>,
    /// Whether the node is currently selected.
    pub selected: Signal<bool>,
    /// The content of the node.
    pub children: ViewRef,

    /// Callback called when the title bar is dragged.
    pub on_drag: Option<Callback<Vec2>>,
}

fn style_node_graph_node(ss: &mut StyleBuilder) {
    ss.display(ui::Display::Flex)
        .flex_direction(ui::FlexDirection::Column)
        .align_items(ui::AlignItems::Stretch)
        .position(ui::PositionType::Absolute);
}

const NODE_BORDER_RADIUS: f32 = 5.;
const NODE_BORDER_WIDTH: f32 = 1.;

fn style_node_graph_node_title(ss: &mut StyleBuilder) {
    ss.border(1)
        .border_color(colors::U4)
        .border(ui::UiRect {
            left: ui::Val::Px(NODE_BORDER_WIDTH),
            right: ui::Val::Px(NODE_BORDER_WIDTH),
            top: ui::Val::Px(NODE_BORDER_WIDTH),
            bottom: ui::Val::Px(0.),
        })
        .border_radius(ui::BorderRadius {
            top_left: ui::Val::Px(NODE_BORDER_RADIUS),
            top_right: ui::Val::Px(NODE_BORDER_RADIUS),
            bottom_left: ui::Val::Px(0.),
            bottom_right: ui::Val::Px(0.),
        })
        .background_color(colors::Y_GREEN.darker(0.05))
        .padding((6, 2));
}

fn style_node_graph_node_content(ss: &mut StyleBuilder) {
    ss.border(1)
        .border_color(colors::U4)
        .border(ui::UiRect {
            left: ui::Val::Px(NODE_BORDER_WIDTH),
            right: ui::Val::Px(NODE_BORDER_WIDTH),
            top: ui::Val::Px(0.),
            bottom: ui::Val::Px(NODE_BORDER_WIDTH),
        })
        .border_radius(ui::BorderRadius {
            top_left: ui::Val::Px(0.),
            top_right: ui::Val::Px(0.),
            bottom_left: ui::Val::Px(NODE_BORDER_RADIUS),
            bottom_right: ui::Val::Px(NODE_BORDER_RADIUS),
        })
        .background_color(colors::U2)
        .padding((6, 2));
}

fn style_node_graph_node_shadow(ss: &mut StyleBuilder) {
    ss.position(ui::PositionType::Absolute)
        .left(-3)
        .top(-3)
        .right(-3)
        .bottom(-3)
        .border_radius(NODE_BORDER_RADIUS + 3.)
        .background_color(Srgba::new(0., 0., 0., 0.7));
}

fn style_node_graph_node_outline(ss: &mut StyleBuilder) {
    ss.position(ui::PositionType::Absolute)
        .left(-3)
        .top(-3)
        .right(-3)
        .bottom(-3)
        .border(2)
        .border_color(colors::FOCUS)
        .border_radius(NODE_BORDER_RADIUS + 3.);
}

impl ViewTemplate for NodeGraphNode {
    fn create(&self, cx: &mut Cx) -> impl Into<ViewRef> {
        let position = self.position;
        let id = cx.create_entity();
        let hovering = cx.create_hover_signal(id);

        Element::<NodeBundle>::for_entity(id)
            .named("NodeGraph::Node")
            .with_styles(style_node_graph_node)
            .create_effect(move |cx, ent| {
                // Update node position.
                let pos = position.get(cx);
                let mut style = cx.world_mut().get_mut::<Style>(ent).unwrap();
                style.left = ui::Val::Px(pos.x);
                style.top = ui::Val::Px(pos.y);
            })
            .with_children((
                Element::<NodeBundle>::new()
                    .named("NodeGraph::Node::Shadow")
                    .with_styles(style_node_graph_node_shadow),
                Element::<NodeBundle>::new()
                    .named("NodeGraph::Node::Title")
                    .with_styles(style_node_graph_node_title)
                    .with_children(self.title.clone()),
                Element::<NodeBundle>::new()
                    .with_styles(style_node_graph_node_content)
                    .with_children(self.children.clone()),
                Cond::new(
                    move |cx| hovering.get(cx),
                    || {
                        Element::<NodeBundle>::new()
                            .named("NodeGraph::Node::Outline")
                            .with_styles(style_node_graph_node_outline)
                    },
                    || (),
                ),
            ))
    }
}
