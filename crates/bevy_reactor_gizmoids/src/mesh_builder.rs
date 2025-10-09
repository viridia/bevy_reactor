use bevy::mesh::{Mesh, PrimitiveTopology};

/// Trait that abstracts the construction of a mesh.
pub trait MeshBuilder {
    /// What topology of mesh is produced by this builder.
    const TOPOLOGY: PrimitiveTopology;

    /// Build the mesh, consuming the builder.
    fn build(self, mesh: &mut Mesh);
}
