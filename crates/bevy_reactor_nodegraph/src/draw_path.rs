use bevy::prelude::*;
use bevy::reflect::TypePath;
use bevy::render::render_resource::*;
use bevy::render::storage::ShaderBuffer;
use bevy::shader::ShaderRef;

/// An element within a stroked path.
#[derive(Debug, Copy, Clone)]
pub enum DrawablePathSegment {
    /// Move to a new position.
    Move(Vec2),
    /// Draw a straight line to a new position.
    Line(Vec2),
    /// Draw a quadratic curve to a new position.
    Quadratic((Vec2, Vec2)),
}

/// Defines a stroked path
#[derive(Debug, Clone)]
pub struct DrawablePath {
    color: Srgba,
    width: f32,
    commands: Vec<DrawablePathSegment>,
}

impl DrawablePath {
    pub fn new(color: Srgba, width: f32) -> Self {
        Self {
            color,
            width,
            commands: Vec::new(),
        }
    }

    pub fn move_to(&mut self, point: Vec2) {
        self.commands.push(DrawablePathSegment::Move(point));
    }

    pub fn line_to(&mut self, point: Vec2) {
        self.commands.push(DrawablePathSegment::Line(point));
    }

    pub fn quadratic_to(&mut self, control: Vec2, point: Vec2) {
        self.commands
            .push(DrawablePathSegment::Quadratic((control, point)));
    }

    pub fn bounds(&self) -> Rect {
        if self.commands.is_empty() {
            return Rect::default();
        }
        let mut bounds = Rect {
            min: Vec2::splat(f32::INFINITY),
            max: Vec2::splat(f32::NEG_INFINITY),
        };
        for segment in &self.commands {
            match segment {
                DrawablePathSegment::Move(point) | DrawablePathSegment::Line(point) => {
                    bounds = bounds.union_point(*point);
                }
                DrawablePathSegment::Quadratic((control, point)) => {
                    bounds = bounds.union_point(*control);
                    bounds = bounds.union_point(*point);
                }
            }
        }
        bounds.inflate(self.width * 0.5)
    }
}

/// Type of drawing operation for each path segment.
enum PathCommandType {
    Move = 0,
    Line = 1,
    Quad1 = 2,
    Quad2 = 3,
}

/// CPU-side representation of a path command. Serialized by hand into the
/// `std430` layout that the shader's `array<PathCommand>` expects (see
/// [`DrawPathMaterial::update`]).
#[derive(Debug, Clone, Copy)]
pub struct PathCommand {
    op: u32,
    point: Vec2,
}

#[derive(AsBindGroup, Asset, TypePath, Debug, Clone)]
pub struct DrawPathMaterial {
    /// Stroke color
    #[uniform(0)]
    pub(crate) color: Vec4,

    /// Stroke width
    #[uniform(1)]
    pub(crate) width: f32,

    /// UI Scale
    #[uniform(2)]
    pub(crate) scale: f32,

    /// Path command buffer
    #[storage(3, read_only)]
    pub(crate) path_commands: Handle<ShaderBuffer>,
    // pub(crate) commands: Vec<PathCommand>,
}

impl DrawPathMaterial {
    pub fn new(path_commands: Handle<ShaderBuffer>) -> Self {
        Self {
            color: Default::default(),
            width: 1.0,
            scale: 1.0,
            path_commands: path_commands.clone(),
        }
    }

    pub fn update(&mut self, path: &DrawablePath, buffers: &mut Assets<ShaderBuffer>) {
        let bounds = path.bounds();
        self.color = path.color.to_vec4();
        self.width = path.width;
        let mut commands: Vec<PathCommand> = Vec::new();
        for segment in &path.commands {
            match segment {
                DrawablePathSegment::Move(point) => {
                    commands.push(PathCommand {
                        op: PathCommandType::Move as u32,
                        point: *point - bounds.min,
                    });
                }
                DrawablePathSegment::Line(point) => {
                    commands.push(PathCommand {
                        op: PathCommandType::Line as u32,
                        point: *point - bounds.min,
                    });
                }
                DrawablePathSegment::Quadratic((control, point)) => {
                    commands.push(PathCommand {
                        op: PathCommandType::Quad1 as u32,
                        point: *control - bounds.min,
                    });
                    commands.push(PathCommand {
                        op: PathCommandType::Quad2 as u32,
                        point: *point - bounds.min,
                    });
                }
            }
        }
        // Serialize into the `std430` layout the shader expects: each element is
        // 16 bytes — `op` (u32) at offset 0, four bytes of padding, then `pos`
        // (vec2<f32>, aligned to 8) at offset 8.
        let mut bytes: Vec<u8> = Vec::with_capacity(commands.len() * 16);
        for cmd in &commands {
            bytes.extend_from_slice(&cmd.op.to_le_bytes());
            bytes.extend_from_slice(&[0u8; 4]);
            bytes.extend_from_slice(&cmd.point.x.to_le_bytes());
            bytes.extend_from_slice(&cmd.point.y.to_le_bytes());
        }
        let mut buffer = buffers.get_mut(self.path_commands.id()).unwrap();
        buffer.clear();
        buffer.extend_from_slice(&bytes);
    }
}

impl Default for DrawPathMaterial {
    fn default() -> Self {
        Self {
            color: Default::default(),
            width: Default::default(),
            scale: 1.0,
            path_commands: Handle::default(),
        }
    }
}

impl UiMaterial for DrawPathMaterial {
    fn fragment_shader() -> ShaderRef {
        "embedded://bevy_reactor_nodegraph/assets/draw_path.wesl".into()
    }
}
