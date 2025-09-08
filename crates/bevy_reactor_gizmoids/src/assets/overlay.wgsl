// #import bevy_core_pipeline::tonemapping::tone_mapping
// #import bevy_pbr::{
//     mesh_view_bindings::view,
//     mesh_functions as mfns,
//     mesh_bindings::mesh,
// }

@group(#{MATERIAL_BIND_GROUP}) @binding(0)
var<uniform> color: vec4<f32>;

struct Vertex {
    @location(0) position: vec3<f32>,
};

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
};

// @vertex
// fn vertex(vertex: Vertex, @builtin(instance_index) instance_index: u32) -> VertexOutput {
//     var out: VertexOutput;
//     out.position = mfns::mesh_position_local_to_clip(
//         mfns,:: get_model_matrix(instance_index),
//         vec4<f32>(vertex.position, 1.0)
//     );

//     return out;
// }

@fragment
fn fragment(
    // @builtin(front_facing) is_front: bool,
    mesh: VertexOutput,
) -> @location(0) vec4<f32> {
    return color;
    // return vec4<f32>(1.0, 1.0, 1.0, 0.5);
    // return tone_mapping(color, view.color_grading);
}
