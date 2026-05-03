use bevy::{
    color::Color,
    ecs::{
        component::Component,
        entity::Entity,
        hierarchy::ChildOf,
        query::Has,
        system::{Commands, Query},
        template::{FromTemplate, OptionTemplate, VecTemplate},
    },
    feathers::cursor::EntityCursor,
    picking::hover::Hovered,
    scene::{Scene, bsn},
    ui::{
        Checked, InteractionDisabled, Node, Pressed,
        widget::{Button, ImageNode, ImageNodeTemplate},
    },
    window::SystemCursorIcon,
};
use bitflags::bitflags;

bitflags! {
    /// Bitflags representing the possible states of a widget
    #[derive(Clone, Copy, Default, PartialEq)]
    #[repr(transparent)]
    pub struct WidgetState: u32 {
        const DISABLED = 1 << 0;
        const HOVERED  = 1 << 1;
        const PRESSED  = 1 << 2;
        const FOCUSED  = 1 << 3;
        const CHECKED  = 1 << 4;
        // const SELECTED = 1 << 5;
    }
}

/// A struct that contains a matching bit pattern,
#[derive(Clone, Copy, Default, FromTemplate)]
pub struct StatePattern {
    /// Bits that must match the value in `required`
    pub mask: WidgetState,
    /// The required value of the masked bits
    pub required: WidgetState,
}

impl StatePattern {
    pub fn matches(&self, state: WidgetState) -> bool {
        (state & self.mask) == (self.required & self.mask)
    }
}

/// Indicates which entity we should look to for the state bits
#[derive(Component, Clone, Copy, PartialEq, Default, FromTemplate)]
pub enum StateSource {
    #[default]
    SelfState,
    Parent,
    Entity(Entity), // escape hatch for weirder cases like radio groups
}

/// A single rule in the list of style rules.
#[derive(Component, Clone, Default, FromTemplate)]
pub struct StyleRule {
    pub pattern: StatePattern,
    #[template(OptionTemplate<ImageNodeTemplate>)]
    pub background: Option<ImageNode>, // None = render nothing for this state
}

/// A component which updates the style of an entity based on the widget states (disabled, pressed,
/// hovered, and so on)
#[derive(Component, Clone, Default, FromTemplate)]
pub struct StatefulStyle {
    pub source: StateSource,
    #[template(VecTemplate<StyleRuleTemplate>)]
    pub rules: Vec<StyleRule>,
}

/// A relationship component used to define multiple visual states of a widget.
#[derive(Component, Clone, PartialEq, Eq, Debug)]
#[relationship(relationship_target = VisualStates)]
pub struct VisualStateOf(pub Entity);

impl VisualStateOf {
    pub fn get(&self) -> Entity {
        self.0
    }
}

impl Default for VisualStateOf {
    fn default() -> Self {
        VisualStateOf(Entity::PLACEHOLDER)
    }
}

/// The set of entities owned by this one, see [`StateOf`].
#[derive(Component, Default)]
#[relationship_target(relationship = VisualStateOf, linked_spawn)]
pub struct VisualStates(Vec<Entity>);

impl core::ops::Deref for VisualStates {
    type Target = [Entity];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// TODO: Use change detection to avoid evaluating rules every frame.
#[allow(clippy::type_complexity)]
pub fn update_widget_styles(
    mut q_styled: Query<(
        Entity,
        &StatefulStyle,
        Option<&ChildOf>,
        Option<&mut ImageNode>,
    )>,
    q_states: Query<(
        &Hovered,
        Has<Pressed>,
        Has<Checked>,
        Has<InteractionDisabled>,
    )>,
    mut commands: Commands,
) {
    for (styled_ent, style, parent, image_node) in q_styled.iter_mut() {
        let source: Entity = match style.source {
            StateSource::SelfState => styled_ent,
            StateSource::Parent => match parent {
                Some(p) => p.parent(),
                None => styled_ent, // Fall back to self if there is no parent.
            },
            StateSource::Entity(entity) => entity,
        };

        if let Ok((hovered, pressed, checked, disabled)) = q_states.get(source) {
            let bits: WidgetState = WidgetState::default()
                | (if hovered.get() {
                    WidgetState::HOVERED
                } else {
                    WidgetState::empty()
                })
                | (if pressed {
                    WidgetState::PRESSED
                } else {
                    WidgetState::empty()
                })
                | (if checked {
                    WidgetState::CHECKED
                } else {
                    WidgetState::empty()
                })
                | (if disabled {
                    WidgetState::DISABLED
                } else {
                    WidgetState::empty()
                });

            // Find first matching rule
            for rule in &style.rules {
                if rule.pattern.matches(bits) {
                    match (image_node, &rule.background) {
                        (Some(mut image_node), Some(new_image)) => {
                            if image_node.color != new_image.color {
                                image_node.color = new_image.color;
                            }
                            if image_node.flip_x != new_image.flip_x {
                                image_node.flip_x = new_image.flip_x;
                            }
                            if image_node.flip_y != new_image.flip_y {
                                image_node.flip_y = new_image.flip_y;
                            }
                            if image_node.image != new_image.image {
                                image_node.image = new_image.image.clone();
                            }
                            if image_node.image_mode != new_image.image_mode {
                                image_node.image_mode = new_image.image_mode.clone();
                            }
                            if image_node.texture_atlas != new_image.texture_atlas {
                                image_node.texture_atlas = new_image.texture_atlas.clone();
                            }
                            if image_node.rect != new_image.rect {
                                image_node.rect = new_image.rect;
                            }
                        }
                        (Some(_), None) => {
                            commands.entity(styled_ent).remove::<ImageNode>();
                        }
                        (None, Some(new_image)) => {
                            commands.entity(styled_ent).insert(new_image.clone());
                        }
                        (None, None) => {}
                    }
                    break;
                }
            }
        }
    }
}

pub fn button() -> impl Scene {
    bsn! {
        Node {}
        Button
        Hovered
        EntityCursor::System(SystemCursorIcon::Pointer)
        StateSource::SelfState
        VisualStates [
            StyleRule {
                pattern: StatePattern {
                    // mask: WidgetState::DISABLED,
                    // required: WidgetState::DISABLED,
                },
                background: {ImageNodeTemplate {
                    color: Color::WHITE,
                    // image: ImageTemplate::Handle(""),
                    ..Default::default()
                }},
            },
            StyleRule {
                pattern: StatePattern {
                    // mask: WidgetState::PRESSED,
                    // required: WidgetState::PRESSED,
                },
                background: ImageNode {
                },
            },
        ]
        // Children [
        //     {props.caption}
        // ]
    }
}
