use std::ops::Mul;

use bevy::{
    asset::Assets,
    color::Color,
    ecs::{
        component::Component,
        lifecycle::Insert,
        observer::On,
        system::{Commands, Query, ResMut},
    },
    log::info,
    math::Vec2,
    render::storage::ShaderStorageBuffer,
    ui::{BorderColor, Node, PositionType, UiRect, Val, px},
    ui_render::prelude::MaterialNode,
};

use crate::{DrawPathMaterial, DrawablePath};

/// Displays a stroked path between two nodes.
#[derive(Component, Clone, Default)]
#[component(immutable)]
#[require(Node)]
pub struct EdgeDisplay {
    /// Pixel position of the source terminal.
    pub src_pos: Vec2,

    /// Pixel position of the destination terminal.
    pub dst_pos: Vec2,

    /// Color of this edge
    pub color: Color,
}

pub fn on_insert_edge(
    insert: On<Insert, EdgeDisplay>,
    mut q_node: Query<(
        &EdgeDisplay,
        &mut Node,
        Option<&MaterialNode<DrawPathMaterial>>,
    )>,
    mut r_materials: ResMut<Assets<DrawPathMaterial>>,
    mut r_bindings: ResMut<Assets<ShaderStorageBuffer>>,
    mut commands: Commands,
) {
    // Adjust node
    if let Ok((edge, mut node, material_node)) = q_node.get_mut(insert.entity) {
        let mut path = DrawablePath::new(edge.color.into(), 1.5);
        let src = edge.src_pos;
        let dst = edge.dst_pos;
        let dx = (dst.x - src.x).abs().mul(0.3).min(20.);
        let src1 = src + Vec2::new(dx, 0.);
        let dst1 = dst - Vec2::new(dx, 0.);
        path.move_to(src);
        let mlen = src1.distance(dst1);
        if mlen > 40. {
            let src2 = src1.lerp(dst1, 20. / mlen);
            let dst2 = src1.lerp(dst1, (mlen - 20.) / mlen);
            path.quadratic_to(src1, src2);
            path.line_to(dst2);
            path.quadratic_to(dst1, dst);
        } else {
            let mid = src1.lerp(dst1, 0.5);
            path.quadratic_to(src1, mid);
            path.quadratic_to(dst1, dst);
        }
        let bounds = path.bounds();

        node.left = Val::Px(bounds.min.x);
        node.top = Val::Px(bounds.min.y);
        node.width = Val::Px(bounds.width());
        node.height = Val::Px(bounds.height());
        node.position_type = PositionType::Absolute;

        match material_node {
            Some(material_handle) => todo!(),
            None => {
                info!("Adding material node {bounds:?}");
                let buffer = r_bindings.add(ShaderStorageBuffer::default());
                let mut material = DrawPathMaterial::new(buffer);
                material.update(&path, &mut r_bindings);
                commands
                    .entity(insert.entity)
                    .insert(MaterialNode(r_materials.add(material)));
            }
        };

        // let material_id = material_node.0.id();
        // let material = r_materials.get_mut(material_id).unwrap();
        // material.update(&path);
        // info!("Entity inserted");
        node.border = UiRect::all(px(2));
        commands
            .entity(insert.entity)
            .insert(BorderColor::all(Color::srgb(0.1, 0.1, 0.2)));
    }
}
