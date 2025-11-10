use bevy::{
    ecs::{entity::Entity, event::EntityEvent, resource::Resource},
    math::{Rect, Vec2, VectorSpace},
};

use crate::ConnectionTerminus;

/// Where we are in the lifecycle of a drag gesture: at the start, the end, or somewhere
/// in the middle.
#[derive(Clone, Copy, Default, Debug, PartialEq)]
pub enum DragAction {
    /// Indicates that we just started dragging.
    #[default]
    Start,
    /// Indicates a drag already in progress.
    InProgress,
    /// The drag operation has completed.
    Finish,
    /// Cancel the drag operation.
    Cancel,
}

/// For a connection drag gesture, where we are dragging from.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ConnectionAnchor {
    /// Drag from an input terminal
    InputTerminal(Entity),
    /// Drag from an output terminal
    OutputTerminal(Entity),
    /// Dragging the source end (connected to an output) of an existing edge.
    EdgeStart(Entity),
    /// Dragging the sink end (connected to an input) of an existing edge.
    EdgeEnd(Entity),
}

/// For a connection drag gesture, the current drag location - what we are hovering over.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ConnectionTarget {
    /// Drag from an input terminal
    InputTerminal(Entity),
    /// Drag from an output terminal
    OutputTerminal(Entity),
    /// Dragging the source end (connected to an output) of an existing edge.
    Location(Vec2),
}

/// A gesture describes the current user interaction currently in progress. A gesture typically
/// represents some kind of drag operation, such as rectangle selection, node movement, connecting
/// edges, or scrolling. However, this can also represent a click action, such as selecting a
/// node or clearing the current selection.
#[derive(Clone, Debug, Default)]
pub enum Gesture {
    /// Drag one or more nodes (ones that are currently selected).
    /// The arguments are the drag vector, and whether this is the final drag value.
    Move,

    /// Drag a node onto the graph to create it.
    Create(Vec2),

    /// Event sent when dragging a connection.
    Connect {
        /// Where we are dragging from
        anchor: ConnectionAnchor,
        /// Where we are dragging to
        target: ConnectionTarget,
    },

    /// Alt-click / Option-click to scroll the view.
    Scroll(Vec2),

    /// Select a rectangular region
    SelectRect { anchor: Vec2, rect: Rect },

    // /// Cancel the current action.
    // Cancel,
    /// Nothing happening
    #[default]
    Idle,
}

/// Event triggered when moving nodes by dragging. All selected nodes will have their
/// [`GraphNodeOffset`] components updated before the first event is dispatched.
#[derive(Clone, EntityEvent, Debug)]
pub struct MoveNodesEvent {
    /// Graph being edited
    #[event_target]
    pub graph: Entity,
    /// Relative drag position
    pub distance: Vec2,
}

/// An event which is sent while dragging connections between terminals.
#[derive(Clone, EntityEvent, Debug)]
pub struct ConnectEvent {
    /// Graph being edited
    #[event_target]
    pub graph: Entity,
    /// Source (output) terminal or location
    pub src: ConnectionTerminus,
    /// Destination (input) terminal or location
    pub dst: ConnectionTerminus,
    /// What phase of dragging we're in
    pub action: DragAction,
}

impl ConnectEvent {
    /// Convert the `anchor` and `target` fields into `src` and `dst`. This may involve
    /// swapping the order if the user is dragging in the reverse direction (from end to start).
    pub fn from_gesture(
        graph: Entity,
        anchor: ConnectionAnchor,
        target: ConnectionTarget,
        action: DragAction,
    ) -> Self {
        let src: ConnectionTerminus;
        let dst: ConnectionTerminus;

        // Sort ends based on terminal type.
        match anchor {
            ConnectionAnchor::InputTerminal(input) => {
                dst = ConnectionTerminus::Terminal(input);
                match target {
                    ConnectionTarget::InputTerminal(_entity) => {
                        panic!("Cannot connect input to input");
                    }
                    ConnectionTarget::OutputTerminal(output) => {
                        assert_ne!(input, output);
                        src = ConnectionTerminus::Terminal(output);
                    }
                    ConnectionTarget::Location(loc) => {
                        src = ConnectionTerminus::Location(loc);
                    }
                }
            }
            ConnectionAnchor::OutputTerminal(output) => {
                src = ConnectionTerminus::Terminal(output);
                match target {
                    ConnectionTarget::InputTerminal(input) => {
                        assert_ne!(input, output);
                        dst = ConnectionTerminus::Terminal(input)
                    }
                    ConnectionTarget::OutputTerminal(_entity) => {
                        panic!("Cannot connect output to output");
                    }
                    ConnectionTarget::Location(loc) => {
                        dst = ConnectionTerminus::Location(loc);
                    }
                }
            }

            ConnectionAnchor::EdgeStart(_entity) => match target {
                ConnectionTarget::InputTerminal(_entity) => todo!(),
                ConnectionTarget::OutputTerminal(_entity) => todo!(),
                ConnectionTarget::Location(_loc) => todo!(),
            },

            ConnectionAnchor::EdgeEnd(_entity) => match target {
                ConnectionTarget::InputTerminal(_entity) => todo!(),
                ConnectionTarget::OutputTerminal(_entity) => todo!(),
                ConnectionTarget::Location(_vec2) => todo!(),
            },
        }

        Self {
            graph,
            src,
            dst,
            action,
        }
    }
}

#[derive(Resource, Default)]
#[allow(dead_code)]
pub(crate) struct GestureState {
    /// The graph we are interacting with.
    pub(crate) graph: Option<Entity>,

    /// The edge we are interacting with.
    pub(crate) gesture: Gesture,

    /// Where we started dragging the selection rect.
    /// TODO: Remove this, use field in gesture instead.
    pub(crate) selection_anchor: Vec2,

    /// The target of the current drag operation. This will be the terminal that the pointer
    /// is hovering over.
    pub(crate) connection_target: Option<ConnectionTarget>,
}
