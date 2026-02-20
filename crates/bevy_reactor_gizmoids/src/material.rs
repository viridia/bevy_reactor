use bevy::{
    asset::{Asset, Assets},
    color::{Alpha as _, Color, LinearRgba, Srgba},
    ecs::{
        change_detection::DetectChanges,
        component::Component,
        entity::Entity,
        hierarchy::ChildOf,
        query::With,
        reflect::ReflectComponent,
        relationship::Relationship,
        system::{Commands, Query, ResMut},
        world::Ref,
    },
    material::AlphaMode,
    mesh::{Mesh3d, MeshVertexBufferLayoutRef},
    pbr::{Material, MaterialPipeline, MaterialPipelineKey, MeshMaterial3d},
    reflect::{Reflect, TypePath, prelude::ReflectDefault},
    render::render_resource::{
        AsBindGroup, CompareFunction, RenderPipelineDescriptor, SpecializedMeshPipelineError,
    },
    shader::ShaderRef,
};

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
    /// Alpha blend mode
    pub alpha_mode: AlphaMode,
}

impl Default for OverlayColor {
    fn default() -> Self {
        Self {
            base: Srgba::RED.into(),
            underlay: 0.3,
            alpha_mode: AlphaMode::Blend,
        }
    }
}

/// Material for overlays
#[derive(Debug, Clone, AsBindGroup, Asset, TypePath, Default)]
pub struct OverlayMaterial {
    #[uniform(0)]
    pub(crate) color: LinearRgba,
    pub(crate) alpha_mode: AlphaMode,
}

#[derive(Component, Default, Clone, Copy)]
pub(crate) struct HasOverlay;

#[derive(Component, Default, Clone, Copy)]
pub(crate) struct HasUnderlay;

#[allow(unused_variables)]
impl Material for OverlayMaterial {
    fn fragment_shader() -> ShaderRef {
        "embedded://bevy_reactor_gizmoids/assets/overlay.wgsl".into()
    }

    fn alpha_mode(&self) -> AlphaMode {
        self.alpha_mode
    }

    fn specialize(
        pipeline: &MaterialPipeline,
        descriptor: &mut RenderPipelineDescriptor,
        layout: &MeshVertexBufferLayoutRef,
        key: MaterialPipelineKey<Self>,
    ) -> Result<(), SpecializedMeshPipelineError> {
        if let Some(ref mut depth_stencil) = descriptor.depth_stencil {
            depth_stencil.depth_write_enabled = false;
            depth_stencil.depth_compare = CompareFunction::GreaterEqual;
        }
        descriptor.primitive.cull_mode = None;
        Ok(())
    }
}

/// Material for occluded overlays
#[derive(Debug, Clone, AsBindGroup, Asset, TypePath, Default)]
pub struct UnderlayMaterial {
    #[uniform(0)]
    pub(crate) color: LinearRgba,
    /// Alpha blend mode
    pub alpha_mode: AlphaMode,
}

#[allow(unused_variables)]
impl Material for UnderlayMaterial {
    fn fragment_shader() -> ShaderRef {
        "embedded://bevy_reactor_gizmoids/assets/overlay.wgsl".into()
    }

    fn alpha_mode(&self) -> AlphaMode {
        self.alpha_mode
    }

    fn specialize(
        pipeline: &MaterialPipeline,
        descriptor: &mut RenderPipelineDescriptor,
        layout: &MeshVertexBufferLayoutRef,
        key: MaterialPipelineKey<Self>,
    ) -> Result<(), SpecializedMeshPipelineError> {
        if let Some(ref mut depth_stencil) = descriptor.depth_stencil {
            depth_stencil.depth_write_enabled = false;
            depth_stencil.depth_compare = CompareFunction::Less;
        }
        descriptor.primitive.cull_mode = None;
        Ok(())
    }
}

#[allow(clippy::type_complexity)]
pub(crate) fn sync_underlay(
    q_overlay: Query<
        (&Mesh3d, Ref<OverlayColor>, &MeshMaterial3d<OverlayMaterial>),
        With<HasOverlay>,
    >,
    q_underlay: Query<
        (
            Entity,
            Option<&Mesh3d>,
            &MeshMaterial3d<UnderlayMaterial>,
            &ChildOf,
        ),
        With<HasUnderlay>,
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
                if let Some(mut material) = r_overlay.get_mut(overlay_material) {
                    material.color = color.base.into();
                    material.alpha_mode = color.alpha_mode;
                }

                if let Some(mut material) = r_underlay.get_mut(underlay_material) {
                    material.color = color
                        .base
                        .with_alpha(color.base.alpha() * color.underlay)
                        .into();
                    material.alpha_mode = color.alpha_mode;
                }
            }
        }
    }
}
