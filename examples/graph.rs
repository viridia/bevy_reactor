//! Example which demonstrates a node graph.

use bevy::{
    color::palettes,
    feathers::{
        FeathersPlugins,
        controls::{SliderProps, slider},
        dark_theme::create_dark_theme,
        theme::{ThemedText, UiTheme},
    },
    prelude::*,
    scene2::{CommandsSpawnScene, bsn, on},
};
use bevy_reactor::ReactorPlugin;
use bevy_reactor_nodegraph::{
    GraphEdge, GraphNode, GraphNodeOffset, GraphNodeSelection, MoveNodesEvent,
    ReactorNodeGraphPlugin, display_graph, display_graph_node, display_graph_node_body,
    display_graph_node_title, input_terminal, output_terminal,
};

fn main() {
    App::new()
        .add_plugins((
            DefaultPlugins.set(ImagePlugin::default_nearest()),
            FeathersPlugins,
            ReactorPlugin,
            ReactorNodeGraphPlugin,
        ))
        .insert_resource(UiTheme(create_dark_theme()))
        .add_systems(Startup, setup_view_root)
        .add_systems(Update, close_on_esc)
        .run();
}

// const DOT_IMAGE: &str = "embedded://bevy_reactor_nodegraph/assets/dot.png";

fn setup_view_root(asset_server: Res<AssetServer>, mut commands: Commands) {
    let image = asset_server.load("embedded://bevy_reactor_nodegraph/assets/dot.png");

    commands.spawn((Camera::default(), Camera2d));
    commands.spawn_scene(bsn!(
        :display_graph()
        Node {
            position_type: PositionType::Absolute,
            left: px(0),
            right: px(0),
            top: px(0),
            bottom: px(0),
        }
        ImageNode {
            color: Color::srgba(0.15, 0.15, 0.2, 1.0),
            image: {image.clone()},
            image_mode: NodeImageMode::Tiled {
                tile_x: true,
                tile_y: true,
                stretch_value: 0.5,
            },
        }
        BackgroundColor(Srgba::new(0.1, 0.1, 0.13, 1.0))
        on(on_drag_nodes)
        // UiTransform {
        //     scale: Vec2::splat(0.7)
        // }
        [
            GraphEdge {
                src_pos: Vec2::new(10.0, 10.0),
                dst_pos: Vec2::new(180.0, 135.0),
                color: Color::srgb(1.0, 0.0, 0.0),
            },

            :display_graph_node(Vec2::new(100.0, 100.0))
            [
                :display_graph_node_title() [
                    Text::new("Node")
                    ThemedText
                ],
                :display_graph_node_body() [
                    output_terminal(palettes::css::ALICE_BLUE.into()) [
                        Text::new("Color")
                        ThemedText
                    ]
                ],
            ],

            :display_graph_node(Vec2::new(200.0, 100.0))
            [
                :display_graph_node_title() [
                    Text::new("Node 2")
                    ThemedText
                ],
                :display_graph_node_body() [
                    :output_terminal(palettes::css::RED.into()) [
                        Text::new("Color")
                        ThemedText
                    ]
                    ,
                    :input_terminal(palettes::css::ALICE_BLUE.into()) [
                        :slider(SliderProps {
                            value: 0.0,
                            min: 0.0,
                            max: 100.0,
                        })
                        Node {
                            align_self: AlignSelf::Stretch
                        }
                    ]
                    ,
                    :slider(SliderProps {
                        value: 0.0,
                        min: 0.0,
                        max: 100.0,
                    })
                    Node {
                        align_self: AlignSelf::Stretch
                    }
                ],
            ],
        ]
    ));
}

fn on_drag_nodes(
    mov: On<MoveNodesEvent>,
    mut q_node: Query<(&mut Node, &GraphNodeSelection, &GraphNodeOffset), With<GraphNode>>,
) {
    for (mut node, selection, &GraphNodeOffset(offset)) in q_node.iter_mut() {
        if selection.selected {
            node.left = px(mov.distance.x + offset.x);
            node.top = px(mov.distance.y + offset.y);
        }
    }
}

pub fn close_on_esc(input: Res<ButtonInput<KeyCode>>, mut exit: MessageWriter<AppExit>) {
    if input.just_pressed(KeyCode::Escape) {
        exit.write(AppExit::Success);
    }
}
