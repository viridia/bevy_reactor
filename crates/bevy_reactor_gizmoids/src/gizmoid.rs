use bevy::{
    asset::RenderAssetUsages,
    ecs::template::template,
    light::{NotShadowCaster, NotShadowReceiver},
    prelude::*,
    scene2::{Scene, bsn},
};
use bevy_reactor::{Cx, effect};

use crate::material::{HasOverlay, HasUnderlay, OverlayColor, OverlayMaterial, UnderlayMaterial};

pub fn gizmoid<
    MB: crate::MeshBuilder + Default + Send + Sync + 'static,
    F: Fn(&Cx, &mut MB) + Clone + Send + Sync + 'static,
>(
    draw: F,
) -> impl Scene {
    bsn! {(
        HasOverlay
        template(|context| {
            Ok(MeshMaterial3d::<OverlayMaterial>(
                context
                    .entity.resource_mut::<Assets<OverlayMaterial>>()
                    .add(OverlayMaterial::default()),
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
            HasUnderlay
            NotShadowCaster
            template(|_| Ok(NotShadowReceiver))
            template(|context| {
                Ok(MeshMaterial3d::<UnderlayMaterial>(
                    context.entity
                        .resource_mut::<Assets<UnderlayMaterial>>()
                        .add(UnderlayMaterial::default()),
                ))
            })
        ]
    )}
}
