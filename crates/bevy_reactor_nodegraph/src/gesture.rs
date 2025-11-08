use bevy::{
    ecs::{entity::Entity, event::EntityEvent, resource::Resource},
    math::{Rect, Vec2},
};

/// Where we are in the lifecycle of a drag gesture: at the start, the end, or somewhere
/// in the middle.
#[derive(Clone, Copy, Default, Debug, PartialEq)]
pub enum DragPhase {
    /// Indicates that we just started dragging.
    #[default]
    Start,
    /// Indicates a drag already in progress.
    InProgress,
    /// The drag operation has completed.
    Finish,
}

/// For a connection drag gesture, where we are dragging from.
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
#[derive(Clone, Debug)]
pub enum Gesture {
    /// Drag one or more nodes (ones that are currently selected).
    /// The arguments are the drag vector, and whether this is the final drag value.
    Move(Vec2, DragPhase),

    /// Drag a node onto the graph to create it.
    Create(Vec2),

    /// Event sent when dragging a connection.
    Connect(ConnectionAnchor, ConnectionTarget, DragPhase),

    /// Alt-click / Option-click to scroll the view.
    Scroll(Vec2),

    /// Select a rectangular region
    SelectRect(Rect, DragPhase),

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

// / An event generated when the user interacts with the graph.
// #[derive(Clone, EntityEvent, Debug)]
// pub struct GraphEvent {
//     /// Event target
//     #[event_target]
//     pub target: Entity,
//     /// The type of gesture.
//     pub gesture: Gesture,
// }

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

#[derive(Resource, Default)]
#[allow(dead_code)]
pub(crate) struct GestureState {
    /// The type of gesture currently in effect.
    // pub(crate) mode: DragMode,

    /// The graph we are interacting with.
    pub(crate) graph: Option<Entity>,

    /// The graph node we are interacting with.
    pub(crate) node: Option<Entity>,

    /// The anchor of the current drag operation, or `None` if there's no connection drag operation
    /// in progress.
    pub(crate) anchor: Option<ConnectionAnchor>,

    /// The target of the current drag operation. This will be the entity that the pointer
    /// is hovering over, or the current pointer location coordinates if not hovering. If no
    /// drag operation is in progress, then it's `None`.
    pub(crate) target: Option<ConnectionTarget>,
}
