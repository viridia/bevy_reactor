//! Example which demonstrates a node graph.

use bevy::{
    color::palettes,
    feathers::{
        FeathersPlugins,
        controls::{SliderProps, slider},
        dark_theme::create_dark_theme,
        theme::UiTheme,
    },
    prelude::*,
    scene2::{CommandsSpawnScene, bsn, on},
    window::CursorIcon,
};
use bevy_reactor::ReactorPlugin;
use bevy_reactor_nodegraph::{
    ConnectEvent, Connection, ConnectionTerminus, DragAction, Graph, GraphNode, GraphNodeOffset,
    GraphNodeSelection, GraphZoom, MoveNodesEvent, ReactorNodeGraphPlugin, Terminal,
    input_terminal, label, node_graph, node_graph_document, node_graph_node, node_graph_node_body,
    node_graph_node_title, output_terminal,
};

#[derive(Resource, Default)]
struct GraphEditState {
    /// Connection being edited
    connection: Option<Entity>,
    /// Terminal which is showing "not allowed" cursor.
    disabled_terminal: Option<Entity>,
    /// Previous cursor icon
    saved_cursor: CursorIcon,
    /// Zoom level for graph
    zoom_level: u32,
}

fn main() {
    App::new()
        .add_plugins((
            DefaultPlugins.set(ImagePlugin::default_nearest()),
            FeathersPlugins,
            ReactorPlugin,
            ReactorNodeGraphPlugin,
        ))
        .insert_resource(UiTheme(create_dark_theme()))
        .init_resource::<GraphEditState>()
        .add_systems(Startup, setup_view_root)
        .add_systems(Update, handle_key_input)
        .run();
}

fn setup_view_root(asset_server: Res<AssetServer>, mut commands: Commands) {
    let image = asset_server.load("embedded://bevy_reactor_nodegraph/assets/dot.png");

    commands.spawn((Camera::default(), Camera2d));
    commands.spawn_scene(bsn!(
        :node_graph()
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
        on(on_move_nodes)
        on(on_connect)
        [
            :node_graph_document()
            [
                :node_graph_node(Vec2::new(100.0, 200.0))
                [
                    :node_graph_node_title() [
                        :label("Node")
                    ]
                    ,
                    :node_graph_node_body() [
                        #out1
                        :output_terminal(palettes::css::ALICE_BLUE.into()) [
                            :label("Color")
                        ]
                    ]
                ],

                :node_graph_node(Vec2::new(700.0, 500.0))
                [
                    :node_graph_node_title() [
                        :label("Bevy!")
                    ]
                    ,
                    :node_graph_node_body() [
                        #out4
                        :output_terminal(palettes::css::ALICE_BLUE.into()) [
                            :label("Position")
                        ]
                    ]
                ],

                :node_graph_node(Vec2::new(300.0, 200.0))
                [
                    :node_graph_node_title() [
                        :label("Node 2")
                    ]
                    ,
                    :node_graph_node_body() [
                        #out2
                        :output_terminal(palettes::css::RED.into()) [
                            :label("Color")
                        ]
                        ,
                        #in1
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
                    ]
                ],

                :node_graph_node(Vec2::new(500.0, 1000.0))
                [
                    :node_graph_node_title() [
                        :label("Node")
                    ]
                    ,
                    :node_graph_node_body() [
                        #out3
                        :output_terminal(palettes::css::ALICE_BLUE.into()) [
                            :label("Color")
                        ]
                    ]
                ],

                // GraphBoundsDebug
                // Node {
                //     position_type: PositionType::Absolute,
                //     left: px(0),
                //     top: px(0),
                //     border: UiRect::all(px(1)),
                // }
                // BorderColor::all(palette::X_AXIS)
                // Pickable::IGNORE
                // effect::effect(|cx: &Cx| {
                //     let parent_id = cx.owner().get::<ChildOf>().unwrap().parent();
                //     let granparent_id = cx.entity(parent_id).get::<ChildOf>().unwrap().parent();
                //     cx.entity(granparent_id).get::<GraphBounds>().unwrap().0
                // }, |ent, rect| {
                //     if !rect.is_empty() {
                //         let mut node = ent.get_mut::<Node>().unwrap();
                //         node.left = px(rect.min.x);
                //         node.top = px(rect.min.y);
                //         node.width = px(rect.width());
                //         node.height = px(rect.height());
                //     }
                // })
            ]
        ]
    ));
}

fn on_move_nodes(
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

fn on_connect(
    connect: On<ConnectEvent>,
    q_terminal: Query<&Terminal>,
    mut r_graph_state: ResMut<GraphEditState>,
    mut commands: Commands,
) {
    let container = connect.connection_list;

    // Check if connection is valid
    let input_terminal = match connect.dst {
        ConnectionTerminus::Terminal(term_ent) => q_terminal
            .get(term_ent)
            .ok()
            .filter(|term| **term == Terminal::Input),
        ConnectionTerminus::Location(_) => None,
    };

    let output_terminal = match connect.src {
        ConnectionTerminus::Terminal(term_ent) => q_terminal
            .get(term_ent)
            .ok()
            .filter(|term| **term == Terminal::Output),
        ConnectionTerminus::Location(_) => None,
    };

    let is_valid = input_terminal.is_some() && output_terminal.is_some();
    // TODO: Check for compatible data types

    let connection: Connection = Connection {
        src: connect.src,
        dst: connect.dst,
        color: Color::srgb(0.5, 1.0, 0.5), // TODO: Change color based on valid
    };

    let mut forbidden_terminal: Option<Entity> = None;
    // let mut show_forbidden = false;

    match connect.action {
        DragAction::StartNew => {
            // Create a new connection entity
            let connection = commands.spawn(connection).id();
            r_graph_state.connection = Some(connection);
            commands.entity(container).add_child(connection);
        }
        DragAction::StartEdit(connection) => {
            r_graph_state.connection = Some(connection);
        }
        DragAction::InProgress => {
            // Update the position of the connection
            let conn_id = r_graph_state.connection.unwrap();
            commands.entity(conn_id).insert(connection);
            if !is_valid {
                forbidden_terminal = None;
            }
            // show_forbidden = !is_valid;
        }
        DragAction::Finish => {
            // If the connection is valid, detach (so it stays around)
            // otherwise, despawn.
            let conn_id = r_graph_state.connection.unwrap();
            if !is_valid {
                // debug!("Connection not valid, despawning");
                commands.entity(conn_id).despawn();
            }
            r_graph_state.connection = None;
        }
        DragAction::Cancel => {
            // Despawn the connection
            let conn_id = r_graph_state.connection.unwrap();
            commands.entity(conn_id).despawn();
            r_graph_state.connection = None;
        }
    }
}

fn handle_key_input(
    input: Res<ButtonInput<KeyCode>>,
    mut exit: MessageWriter<AppExit>,
    mut q_graph: Query<&mut GraphZoom, With<Graph>>,
    mut r_graph_state: ResMut<GraphEditState>,
) {
    if input.just_pressed(KeyCode::Escape) {
        exit.write(AppExit::Success);
    } else if input.just_pressed(KeyCode::Equal) {
        r_graph_state.zoom_level = r_graph_state.zoom_level.saturating_sub(1);
        for mut graph_zoom in q_graph.iter_mut() {
            graph_zoom.0 = 0.8f32.powf(r_graph_state.zoom_level as f32);
        }
    } else if input.just_pressed(KeyCode::Minus) {
        r_graph_state.zoom_level = r_graph_state.zoom_level.saturating_add(1);
        for mut graph_zoom in q_graph.iter_mut() {
            graph_zoom.0 = 0.8f32.powf(r_graph_state.zoom_level as f32);
        }
    }
}
