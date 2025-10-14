use bevy::{
    ecs::{entity::Entity, event::EntityEvent, resource::Resource},
    math::{Rect, Vec2},
};

/// TODO: document
#[derive(Clone, Copy, Default, Debug, PartialEq)]
pub enum DragAction {
    /// Indicates that we just started dragging.
    #[default]
    Start,
    /// Indicates a drag already in progress.
    InProgress,
    /// The drag operation has completed.
    Finish,
}

/// For a connection drag, where we are dragging from.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ConnectionAnchor {
    /// Drag from an input terminal
    InputTerminal(Entity),
    /// Drag from an output terminal
    OutputTerminal(Entity),
    /// Dragging the source end (connected to an output) of an existing edge.
    EdgeSource(Entity),
    /// Dragging the sink end (connected to an input) of an existing edge.
    EdgeSink(Entity),
}

/// For a connection drag, the current drag location.
#[derive(Clone, Copy, Debug, PartialEq, Default)]
pub enum ConnectionTarget {
    /// Drag from an input terminal
    InputTerminal(Entity),
    /// Drag from an output terminal
    OutputTerminal(Entity),
    /// Dragging the source end (connected to an output) of an existing edge.
    Location(Vec2),
    /// Not dragging
    #[default]
    None,
}

/// A gesture is an object that describes the current in-progress interaction that the user
/// is engaging in.
#[derive(Clone, Debug)]
pub enum Gesture {
    /// Drag one or more nodes (ones that are currently selected).
    /// The arguments are the drag vector, and whether this is the final drag value.
    Move(Vec2, DragAction),

    /// Drag a node onto the graph to create it.
    Create(Vec2),

    /// Event sent when dragging a connection.
    Connect(ConnectionAnchor, ConnectionTarget, DragAction),

    /// Option-click to scroll the view.
    Scroll(Vec2),

    /// Select a rectangular region
    SelectRect(Rect, DragAction),

    /// Select the given node. If the node is already selected, does nothing. If the node is
    /// not selected, clears the selection and selects only the given node.
    Select(Entity),

    /// Add a node to the selection, don't affect other nodes.
    SelectAdd(Entity),

    /// Remove a node from the selection.
    SelectRemove(Entity),

    /// Toggle the selection state of a node.
    SelectToggle(Entity),

    /// Remove all nodes from the selection.
    SelectClear,

    /// Cancel the current action.
    Cancel,
}

/// An event generated when the user interacts with the graph.
#[derive(Clone, EntityEvent, Debug)]
pub struct GraphEvent {
    /// Event target
    #[event_target]
    pub target: Entity,
    /// The type of gesture.
    pub gesture: Gesture,
}

#[derive(Resource, Default)]
pub(crate) struct GestureState {
    /// The type of gesture currently in effect.
    // pub(crate) mode: DragMode,

    /// The graph we are interacting with.
    pub(crate) graph: Option<Entity>,

    /// The anchor of the current drag operation.
    pub(crate) anchor: Option<ConnectionAnchor>,

    /// The target of the current drag operation.
    pub(crate) target: ConnectionTarget,
}
