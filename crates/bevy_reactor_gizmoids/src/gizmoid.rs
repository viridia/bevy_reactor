use bevy::{
    asset::RenderAssetUsages,
    ecs::{
        component::Component, reflect::ReflectComponent, relationship::Relationship,
        template::template,
    },
    pbr::{NotShadowCaster, NotShadowReceiver},
    prelude::*,
    reflect::{Reflect, prelude::ReflectDefault},
    scene2::{Scene, bsn},
};
use bevy_reactor::{Cx, effect};

use crate::material::{OverlayMaterial, UnderlayMaterial};

/// Component that determines the fill color of the gizmoid.
#[derive(Component, Clone, Copy, Debug, PartialEq, Reflect)]
#[reflect(Component, Default)]
#[component(immutable)]
pub struct OverlayColor {
    /// Base color of gizmoid
    pub base: Color,
    /// Occlusion opacity, 0.0 to 1.0. This represents the opacity of the gizmoid when it is
    /// occluded by other objects. If not present, then the gizmoid will be completely occluded
    /// by other objects.
    pub underlay: f32,
}

impl Default for OverlayColor {
    fn default() -> Self {
        Self {
            base: Srgba::RED.into(),
            underlay: 0.3,
        }
    }
}

#[derive(Component, Default, Clone, Copy)]
pub(crate) struct GizmoidOverlay;

#[derive(Component, Default, Clone, Copy)]
pub(crate) struct GizmoidUnderlay;

pub fn gizmoid<
    MB: crate::MeshBuilder + Default + Send + Sync + 'static,
    F: Fn(&Cx, &mut MB) + Clone + Send + Sync + 'static,
>(
    draw: F,
) -> impl Scene {
    bsn! {(
        GizmoidOverlay
        template(|context| {
            Ok(MeshMaterial3d::<OverlayMaterial>(
                context
                    .resource_mut::<Assets<OverlayMaterial>>()
                    .add(OverlayMaterial { color: default() }),
            ))
        })
        // This used to work.
        // template(|context| {
        //     Ok(MeshMaterial3d::<UnderlayMaterial>(
        //         context
        //             .resource_mut::<Assets<UnderlayMaterial>>()
        //             .add(UnderlayMaterial { color: RED.with_alpha(0.5).into() }),
        //     ))
        // })
        OverlayColor
        NotShadowCaster
        template(|_| Ok(NotShadowReceiver))

        // Mesh geometry reaction
        effect::effect(move |cx: &Cx| {
            let mut mb = MB::default();
            draw(cx, &mut mb);
            mb
         }, |entity, builder| {
            // See if we already have a mesh component.
            let mesh_cmp = entity.get::<Mesh3d>();
            match mesh_cmp {
                Some(Mesh3d(mesh_handle)) => {
                    // Update the existing mesh handle
                    let mesh_handle = mesh_handle.clone();
                    entity.world_scope(|world| {
                        let mut meshes = world.get_resource_mut::<Assets<Mesh>>().unwrap();
                        let mesh = meshes.get_mut(mesh_handle.id()).unwrap();
                        builder.build(mesh);
                    });
                },
                _ => {
                    // Create a new mesh and insert it.
                    let handle = entity.world_scope(|world| {
                        let mut meshes = world.get_resource_mut::<Assets<Mesh>>().unwrap();
                        let mut mesh = Mesh::new(MB::TOPOLOGY, RenderAssetUsages::default());
                        builder.build(&mut mesh);
                        meshes.add(mesh)
                    });
                    entity.insert(Mesh3d(handle.clone()));
                },
            }
        })

        [
            GizmoidUnderlay
            NotShadowCaster
            template(|_| Ok(NotShadowReceiver))
            template(|context| {
                Ok(MeshMaterial3d::<UnderlayMaterial>(
                    context
                        .resource_mut::<Assets<UnderlayMaterial>>()
                        .add(UnderlayMaterial { color: default() }),
                ))
            })
        ]
    )}
}

#[allow(clippy::type_complexity)]
pub(crate) fn sync_gizmoid_underlay(
    q_overlay: Query<
        (&Mesh3d, Ref<OverlayColor>, &MeshMaterial3d<OverlayMaterial>),
        With<GizmoidOverlay>,
    >,
    q_underlay: Query<
        (
            Entity,
            Option<&Mesh3d>,
            &MeshMaterial3d<UnderlayMaterial>,
            &ChildOf,
        ),
        With<GizmoidUnderlay>,
    >,
    mut r_overlay: ResMut<Assets<OverlayMaterial>>,
    mut r_underlay: ResMut<Assets<UnderlayMaterial>>,
    mut commands: Commands,
) {
    for (underlay_id, mesh, underlay_material, parent) in q_underlay {
        if let Ok((parent_mesh, color, overlay_material)) = q_overlay.get(parent.get()) {
            if mesh.is_none() {
                commands.entity(underlay_id).insert(parent_mesh.clone());
            }

            if color.is_changed() {
                if let Some(material) = r_overlay.get_mut(overlay_material) {
                    material.color = color.base.into();
                }

                if let Some(material) = r_underlay.get_mut(underlay_material) {
                    material.color = color
                        .base
                        .with_alpha(color.base.alpha() * color.underlay)
                        .into();
                }
            }
        }
    }
}
