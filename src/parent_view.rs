use crate::{Fragment, ViewHandle};
use bevy::ecs::{entity::Entity, world::World};
use impl_trait_for_tuples::*;

/// A view which can contain child views. This will generate child entities when spawned.
pub trait ParentView: Sized {
    /// Get the child views for this element.
    fn children(&self) -> &Vec<ChildView>;

    /// Get a mutable reference to the child views for this element.
    fn children_mut(&mut self) -> &mut Vec<ChildView>;

    /// Return a flat list of child entities derived from the child views.
    fn child_nodes(&self) -> Vec<Entity> {
        let mut count: usize = 0;
        for child in self.children().iter() {
            count += child.view.nodes().count();
        }

        let mut flat: Vec<Entity> = Vec::with_capacity(count);
        for child in self.children().iter() {
            child.view.nodes().flatten(&mut flat);
        }

        flat
    }

    /// Set the child views for this element.
    fn with_children<V: ChildViewTuple>(mut self, views: V) -> Self {
        if !self.children().is_empty() {
            panic!("Children already set");
        }
        self.children_mut()
            .extend(views.to_vec().iter().map(|v| ChildView {
                view: v.clone(),
                entity: None,
            }));
        self
    }

    /// Set a single child view for this element.
    fn with_child(mut self, view: &ViewHandle) -> Self {
        if !self.children().is_empty() {
            panic!("Children already set");
        }
        self.children_mut().push(ChildView {
            view: view.clone(),
            entity: None,
        });
        self
    }

    /// Add a child views to this element.
    fn append_child(mut self, view: &ViewHandle) -> Self {
        self.children_mut().push(ChildView {
            view: view.clone(),
            entity: None,
        });
        self
    }

    /// Raze all child views.
    fn raze_children(&mut self, world: &mut World) {
        // Raze all child views
        for child in self.children_mut().drain(..) {
            // Calling `raze` on the child view will despawn the child entity.
            let inner = child.view;
            inner.raze(child.entity.unwrap(), world);
        }
    }
}

/// Used by widgets to track the entities created by their children.
pub struct ChildView {
    /// The view handle for generating the child entity.
    pub view: ViewHandle,
    /// The entity id for the child entity.
    pub entity: Option<Entity>,
}

/// A tuple of [`View`]s which can be converted into a [`Vec<ViewHandle>`].
#[doc(hidden)]
pub trait ChildViewTuple {
    #[doc(hidden)]
    fn get_handles(self, out: &mut Vec<ViewHandle>);

    fn to_vec(self) -> Vec<ViewHandle>;

    /// Convert this tuple of views into a [`ViewHandle`] containing a [`Fragment`].
    fn fragment(self) -> ViewHandle;
}

impl<I: Into<ViewHandle>> ChildViewTuple for I {
    fn get_handles(self, out: &mut Vec<ViewHandle>) {
        out.push(self.into());
    }

    fn to_vec(self) -> Vec<ViewHandle> {
        let mut out = Vec::new();
        self.get_handles(&mut out);
        out
    }

    fn fragment(self) -> ViewHandle {
        ViewHandle::new(Fragment::new(self))
    }
}

#[impl_for_tuples(1, 15)]
#[tuple_types_custom_trait_bound(ChildViewTuple)]
impl ChildViewTuple for Tuple {
    fn get_handles(self, out: &mut Vec<ViewHandle>) {
        for_tuples!(#( self.Tuple.get_handles(out); )*)
    }

    fn to_vec(self) -> Vec<ViewHandle> {
        let mut out = Vec::new();
        self.get_handles(&mut out);
        out
    }

    fn fragment(self) -> ViewHandle {
        ViewHandle::new(Fragment::new(self))
    }
}
