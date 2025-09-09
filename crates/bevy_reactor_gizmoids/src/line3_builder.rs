use bevy::{
    math::{
        Vec3,
        primitives::{Cuboid, Sphere},
    },
    mesh::Meshable,
    render::{
        mesh::{Indices, Mesh, PrimitiveTopology},
        primitives::MeshAabb,
    },
};

use super::mesh_builder::MeshBuilder;

// /// A marker for the start or end of a shape.
// #[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
// pub enum StrokeMarker {
//     /// No marker.
//     #[default]
//     None,

//     /// Arrowhead marker.
//     Arrowhead,
//     // Future: Diamond, Circle, Square, etc.
// }

// /// Options for drawing a polygon or polyline stroke.
// #[derive(Clone, Debug)]
// pub struct PolygonOptions {
//     /// Whether the polygon should be closed.
//     pub closed: bool,

//     /// Line style: length of the dash.
//     pub dash_length: f32,

//     /// Line style: length of the gap between dashes.
//     pub gap_length: f32,

//     /// Marker at the start of the polyline.
//     pub start_marker: StrokeMarker,

//     /// Marker at the end of the polyline.
//     pub end_marker: StrokeMarker,
// }

// impl Default for PolygonOptions {
//     fn default() -> Self {
//         Self {
//             closed: false,
//             dash_length: f32::MAX,
//             gap_length: 0.,
//             start_marker: StrokeMarker::None,
//             end_marker: StrokeMarker::None,
//         }
//     }
// }

/// A builder for creating two-dimensional shapes.
#[derive(Clone, Debug, Default)]
pub struct Line3dBuilder {
    vertices: Vec<Vec3>,
    indices: Vec<u32>,
}

impl Line3dBuilder {
    /// Create a new [`Line3dBuilder`].
    pub fn new() -> Self {
        Self {
            vertices: Vec::new(),
            indices: Vec::new(),
        }
    }

    /// Reserve space for vertices and indices.
    pub fn reserve(&mut self, vertices: usize, indices: usize) -> &mut Self {
        self.vertices.reserve(vertices);
        self.indices.reserve(indices);
        self
    }

    /// Add a vertex to the shape.
    #[inline]
    pub fn push_vertex(&mut self, x: f32, y: f32, z: f32) -> &mut Self {
        self.vertices.push(Vec3::new(x, y, z));
        self
    }

    /// Add an index to the shape.
    #[inline]
    pub fn push_index(&mut self, index: u32) -> &mut Self {
        self.indices.push(index);
        self
    }

    /// Add an index to the shape.
    #[inline]
    pub fn push_indices(&mut self, indices: &[u32]) -> &mut Self {
        self.indices.extend(indices);
        self
    }

    pub fn draw_line(&mut self, start: Vec3, end: Vec3) -> &mut Self {
        let next_index = self.vertices.len() as u32;
        self.vertices.push(start);
        self.vertices.push(end);
        self.indices.push(next_index);
        self.indices.push(next_index + 1);
        self
    }

    pub fn draw_cuboid(&mut self, center: Vec3, cuboid: Cuboid) -> &mut Self {
        let next_index = self.vertices.len() as u32;
        let hs = cuboid.half_size;
        self.vertices.push(center + Vec3::new(hs.x, hs.y, hs.z));
        self.vertices.push(center + Vec3::new(-hs.x, hs.y, hs.z));
        self.vertices.push(center + Vec3::new(hs.x, -hs.y, hs.z));
        self.vertices.push(center + Vec3::new(-hs.x, -hs.y, hs.z));
        self.vertices.push(center + Vec3::new(hs.x, hs.y, -hs.z));
        self.vertices.push(center + Vec3::new(-hs.x, hs.y, -hs.z));
        self.vertices.push(center + Vec3::new(hs.x, -hs.y, -hs.z));
        self.vertices.push(center + Vec3::new(-hs.x, -hs.y, -hs.z));

        // Top face edges
        self.indices.push(next_index);
        self.indices.push(next_index + 1);

        self.indices.push(next_index);
        self.indices.push(next_index + 2);

        self.indices.push(next_index + 1);
        self.indices.push(next_index + 3);

        self.indices.push(next_index + 2);
        self.indices.push(next_index + 3);

        // Bottom face edges
        self.indices.push(next_index + 4);
        self.indices.push(next_index + 5);

        self.indices.push(next_index + 4);
        self.indices.push(next_index + 6);

        self.indices.push(next_index + 5);
        self.indices.push(next_index + 7);

        self.indices.push(next_index + 6);
        self.indices.push(next_index + 7);

        // Connecting edges between top and bottom
        self.indices.push(next_index);
        self.indices.push(next_index + 4);

        self.indices.push(next_index + 1);
        self.indices.push(next_index + 5);

        self.indices.push(next_index + 2);
        self.indices.push(next_index + 6);

        self.indices.push(next_index + 3);
        self.indices.push(next_index + 1);

        self
    }

    // pub fn draw_uv_sphere(&mut self, sphere: Sphere, sectors: u32, stacks: u32) -> &mut Self {
    //     let mut next_index = self.vertices.len() as u32;
    //     for i in 0..stacks + 1 {
    //         // let m = sphere.mesh();
    //     }
    //     self
    // }

    // Primitives:
    // * UVSphere
    // * Icosphere
    // * Capsule
    // * Box
    // * Quad
    // * Triangle
    // * NGon
    // * Torus
    // * Cylinder
    // * Cone (possibly truncated)
    // * Tetra

    // /// Draw a stroke in the shape of a rectangle.
    // ///
    // /// Arguments:
    // /// `rect` - The outer bounds of the rectangle.
    // pub fn stroke_rect(&mut self, rect: Rect) -> &mut Self {
    //     self.reserve(8, 24);

    //     let start = self.vertices.len() as u32;
    //     let lw = self.stroke_width;

    //     self.push_vertex(rect.min.x + lw, rect.min.y + lw, 0.);
    //     self.push_vertex(rect.min.x, rect.min.y, 0.);

    //     self.push_vertex(rect.max.x - lw, rect.min.y + lw, 0.);
    //     self.push_vertex(rect.max.x, rect.min.y, 0.);

    //     self.push_vertex(rect.max.x - lw, rect.max.y - lw, 0.);
    //     self.push_vertex(rect.max.x, rect.max.y, 0.);

    //     self.push_vertex(rect.min.x + lw, rect.max.y - lw, 0.);
    //     self.push_vertex(rect.min.x, rect.max.y, 0.);

    //     self.push_index(start);
    //     self.push_index(start + 1);
    //     self.push_index(start + 2);

    //     self.push_index(start + 1);
    //     self.push_index(start + 3);
    //     self.push_index(start + 2);

    //     self.push_index(start + 2);
    //     self.push_index(start + 3);
    //     self.push_index(start + 4);

    //     self.push_index(start + 4);
    //     self.push_index(start + 3);
    //     self.push_index(start + 5);

    //     self.push_index(start + 4);
    //     self.push_index(start + 5);
    //     self.push_index(start + 6);

    //     self.push_index(start + 5);
    //     self.push_index(start + 7);
    //     self.push_index(start + 6);

    //     self.push_index(start + 6);
    //     self.push_index(start + 1);
    //     self.push_index(start);

    //     self.push_index(start + 6);
    //     self.push_index(start + 7);
    //     self.push_index(start + 1);

    //     self
    // }

    // /// Draw a circular stroke.
    // pub fn stroke_circle(&mut self, center: Vec2, radius: f32, segments: u32) -> &mut Self {
    //     self.reserve((segments * 2) as usize, (segments * 6) as usize);
    //     let start = self.vertices.len() as u32;
    //     let step = 2.0 * std::f32::consts::PI / segments as f32;
    //     let radius_inner = (radius - self.stroke_width).max(0.0);
    //     let radius_outer = radius_inner + self.stroke_width;
    //     for i in 0..segments {
    //         let angle = i as f32 * step;
    //         let c = angle.cos();
    //         let s = angle.sin();
    //         let x_inner = center.x + radius_inner * c;
    //         let y_inner = center.y + radius_inner * s;
    //         let x_outer = center.x + radius_outer * c;
    //         let y_outer = center.y + radius_outer * s;
    //         let next_index = (i + 1).rem_euclid(segments);
    //         self.push_vertex(x_inner, y_inner, 0.);
    //         self.push_vertex(x_outer, y_outer, 0.);

    //         self.push_index(start + i * 2);
    //         self.push_index(start + i * 2 + 1);
    //         self.push_index(start + next_index * 2);

    //         self.push_index(start + i * 2 + 1);
    //         self.push_index(start + next_index * 2 + 1);
    //         self.push_index(start + next_index * 2);
    //     }
    //     self
    // }

    // /// Draw a polygon from a list of points.
    // pub fn draw_polyline(&mut self, vertices: &[Vec2], options: PolygonOptions) -> &mut Self {
    //     if vertices.len() < 2 {
    //         return self;
    //     }
    //     let closed = options.closed && vertices.len() > 2;
    //     let lw = self.stroke_width * 0.5;
    //     let count = vertices.len();

    //     let mut dash_end = options.dash_length;

    //     // Indices of the vertices at the start of the current dash.
    //     let mut v0_index: u32 = 0;
    //     let mut v1_index: u32 = 0;

    //     for i in 0..count {
    //         let vtx = vertices[i];
    //         let vtx_next = vertices[(i + 1).rem_euclid(count)];

    //         // Length and direction of line segment
    //         let mut length = vtx.distance(vtx_next);
    //         let v_dir = (vtx_next - vtx) / length;
    //         let v_perp = Vec2::new(v_dir.y, -v_dir.x).normalize() * lw;

    //         if i == 0 {
    //             // Generate vertices for the start of first segment.
    //             if closed {
    //                 // Mitered starting point.
    //                 let vtx_prev = *vertices.last().unwrap();
    //                 let v_dir_prev = (vtx - vtx_prev).normalize();
    //                 let dot = (v_dir + v_dir_prev).normalize().dot(v_dir_prev);
    //                 let v_miter =
    //                     Vec2::new(v_dir_prev.y + v_dir.y, -v_dir_prev.x - v_dir.x).normalize() * lw
    //                         / dot;
    //                 let v2_index = self.push_vec2_index(vtx + v_miter);
    //                 let v3_index = self.push_vec2_index(vtx - v_miter);
    //                 self.push_indices(&[
    //                     v0_index, v2_index, v1_index, v1_index, v2_index, v3_index,
    //                 ]);
    //                 v0_index = v2_index;
    //                 v1_index = v3_index;
    //                 // todo!();
    //             } else {
    //                 // Draw start marker and update position.
    //                 let marker_length = self.marker_length(options.start_marker).min(length * 0.4);
    //                 self.fill_marker(
    //                     options.start_marker,
    //                     vtx + v_dir * marker_length,
    //                     -v_dir,
    //                     marker_length,
    //                 );
    //                 v0_index = self.push_vec2_index(vtx + v_perp + v_dir * marker_length);
    //                 v1_index = self.push_vec2_index(vtx - v_perp + v_dir * marker_length);
    //                 dash_end += marker_length;
    //             }
    //         }

    //         // If the segment ends in a marker, reduce the line segment length.
    //         let marker_length = self.marker_length(options.end_marker).min(length * 0.4);
    //         if i == count - 2 && !closed {
    //             length -= marker_length;
    //         }

    //         while dash_end < length {
    //             // Finish the previous dash.
    //             let v_dash_end = vtx + v_dir * dash_end.min(length);
    //             let v2_index = self.push_vec2_index(v_dash_end + v_perp);
    //             let v3_index = self.push_vec2_index(v_dash_end - v_perp);
    //             self.push_indices(&[v0_index, v2_index, v1_index, v1_index, v2_index, v3_index]);

    //             // Start a new dash if there's room
    //             if dash_end + options.gap_length < length {
    //                 let v_dash_start = vtx + v_dir * (dash_end + options.gap_length);
    //                 v0_index = self.push_vec2_index(v_dash_start + v_perp);
    //                 v1_index = self.push_vec2_index(v_dash_start - v_perp);
    //             }

    //             // Prep for next dash
    //             dash_end += options.dash_length + options.gap_length;
    //         }

    //         // Miter at end, if it's in the middle of a dash.
    //         if dash_end - options.dash_length < length {
    //             if i < count - 2 || options.closed {
    //                 // Mitered angle.
    //                 let vtx_next2 = vertices[(i + 2).rem_euclid(count)];
    //                 let v_dir_next = (vtx_next2 - vtx_next).normalize();
    //                 let dot = (v_dir_next + v_dir).normalize().dot(v_dir);
    //                 let v_miter =
    //                     Vec2::new(v_dir.y + v_dir_next.y, -v_dir.x - v_dir_next.x).normalize() * lw
    //                         / dot;
    //                 let v2_index = self.push_vec2_index(vtx_next + v_miter);
    //                 let v3_index = self.push_vec2_index(vtx_next - v_miter);
    //                 self.push_indices(&[
    //                     v0_index, v2_index, v1_index, v1_index, v2_index, v3_index,
    //                 ]);
    //                 v0_index = v2_index;
    //                 v1_index = v3_index;
    //             } else {
    //                 // Butt end
    //                 let v_seg_end = vtx + v_dir * length;
    //                 let v2 = v_seg_end + v_perp;
    //                 let v3 = v_seg_end - v_perp;
    //                 let v2_index = self.push_vec2_index(v2);
    //                 let v3_index = self.push_vec2_index(v3);
    //                 self.push_indices(&[
    //                     v0_index, v2_index, v1_index, v1_index, v2_index, v3_index,
    //                 ]);
    //                 self.fill_marker(options.end_marker, v_seg_end, v_dir, marker_length);
    //                 break;
    //             }
    //         }

    //         dash_end -= length;
    //     }
    //     self
    // }

    // /// Add a vertex to the shape, and return the index of that vertex.
    // #[inline]
    // fn push_vec2_index(&mut self, v: Vec2) -> u32 {
    //     let index = self.vertices.len() as u32;
    //     self.push_vertex(v.x, v.y, 0.);
    //     index
    // }

    // fn fill_marker(&mut self, marker: StrokeMarker, position: Vec2, direction: Vec2, length: f32) {
    //     #[allow(clippy::single_match)]
    //     match marker {
    //         StrokeMarker::Arrowhead => {
    //             let v_perp = Vec2::new(direction.y, -direction.x).normalize() * length;
    //             let v0 = position + direction * length;
    //             let v1 = position + v_perp;
    //             let v2 = position - v_perp;
    //             self.fill_triangle(v0, v2, v1);
    //         }
    //         _ => {}
    //     }
    // }

    // /// Compute the length of the stroke marker, relative to the stroke width.
    // fn marker_length(&self, marker: StrokeMarker) -> f32 {
    //     match marker {
    //         StrokeMarker::Arrowhead => self.stroke_width * 2.0,
    //         _ => 0.0,
    //     }
    // }
}

impl MeshBuilder for Line3dBuilder {
    const TOPOLOGY: PrimitiveTopology = PrimitiveTopology::LineList;

    /// Copy the shape into a [`Mesh`]. This will consume the builder and return a mesh.
    fn build(self, mesh: &mut Mesh) {
        mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, self.vertices);
        mesh.insert_indices(Indices::U32(self.indices));
        mesh.compute_aabb();
    }
}
