use std::sync::Arc;

use bevy::reflect::{Access, DynamicList, List as _, OffsetAccess, ReflectMut};
use bevy::{
    ecs::{
        component::{Component, Mutable},
        entity::Entity,
        resource::Resource,
        world::{DeferredWorld, World},
    },
    log::info,
    reflect::{
        DynamicEnum, DynamicVariant, GetPath, ParsedPath, PartialReflect, Reflect,
        ReflectPathError, attributes::CustomAttributes,
    },
};
use bevy_reactor::Cx;

/// Trait that represents a top-level item that can be inspected. "Top level" means that
/// this is a resource, component, or other allocated object that is not a subfield.
#[allow(unused_variables)]
pub trait InspectableRoot: Send + Sync {
    /// The name of the item being inspected
    fn name(&self, cx: &Cx) -> String;

    /// Get the reflected data for a path within the reflected item.
    fn get_reflect<'a>(
        &self,
        world: &'a World,
        path: &ParsedPath,
    ) -> Option<&'a dyn PartialReflect>;

    /// Get the reflected data for a path within the reflected item. This version adds any
    /// component or resource references to the tracking scope.
    fn get_reflect_tracked<'a>(
        &self,
        cx: &'a Cx,
        path: &ParsedPath,
    ) -> Option<&'a dyn PartialReflect>;

    /// Update a field within the inspected object.
    fn set_value(&self, world: &mut DeferredWorld, path: &ParsedPath, value: &dyn PartialReflect);

    /// Apply a closure to a field within the inspected object.
    fn update_value(
        &self,
        world: &mut DeferredWorld,
        path: &ParsedPath,
        f: &dyn Fn(&mut dyn PartialReflect),
    );
}

/// A resource that can be inspected
pub struct InspectableResource<T: Resource + Reflect> {
    marker: std::marker::PhantomData<T>,
}

impl<T: Resource + Reflect> Default for InspectableResource<T> {
    fn default() -> Self {
        Self {
            marker: std::marker::PhantomData,
        }
    }
}

impl<T: Resource + Reflect> InspectableRoot for InspectableResource<T> {
    fn name(&self, cx: &Cx) -> String {
        let res = cx.resource::<T>();
        res.reflect_short_type_path().to_string()
    }

    fn get_reflect_tracked<'a>(
        &self,
        cx: &'a Cx,
        path: &ParsedPath,
    ) -> Option<&'a dyn PartialReflect> {
        let res = cx.resource::<T>();
        match res.reflect_path(path) {
            Ok(result) => Some(result),
            Err(ReflectPathError::InvalidAccess(_)) => None,
            Err(err) => panic!("{err}"),
        }
    }

    /// Return the reflect data for a path within the inspected object.
    fn get_reflect<'a>(
        &self,
        world: &'a World,
        path: &ParsedPath,
    ) -> Option<&'a dyn PartialReflect> {
        let res = world.resource::<T>();
        match res.reflect_path(path) {
            Ok(result) => Some(result),
            Err(ReflectPathError::InvalidAccess(_)) => None,
            Err(err) => panic!("{err}"),
        }
    }

    fn set_value(&self, world: &mut DeferredWorld, path: &ParsedPath, value: &dyn PartialReflect) {
        let mut res = world.get_resource_mut::<T>().unwrap();
        res.reflect_path_mut(path).unwrap().apply(value);
    }

    fn update_value(
        &self,
        world: &mut DeferredWorld,
        path: &ParsedPath,
        f: &dyn Fn(&mut dyn PartialReflect),
    ) {
        let mut res = world.get_resource_mut::<T>().unwrap();
        f(res.reflect_path_mut(path).unwrap());
    }
}

/// An ECS component that can be inspected
pub struct InspectableComponent<T: Component + Reflect> {
    entity: Entity,
    marker: std::marker::PhantomData<T>,
}

impl<T: Component + Reflect> InspectableComponent<T> {
    pub fn new(entity: Entity) -> Self {
        Self {
            entity,
            marker: std::marker::PhantomData,
        }
    }
}

impl<T: Component<Mutability = Mutable> + Reflect> InspectableRoot for InspectableComponent<T> {
    fn name(&self, cx: &Cx) -> String {
        let cmp = cx.component::<T>(self.entity).unwrap();
        cmp.reflect_short_type_path().to_string()
    }

    fn get_reflect_tracked<'a>(
        &self,
        cx: &'a Cx,
        path: &ParsedPath,
    ) -> Option<&'a dyn PartialReflect> {
        let cmp = cx.component::<T>(self.entity).unwrap();
        match cmp.reflect_path(path) {
            Ok(result) => Some(result),
            Err(ReflectPathError::InvalidAccess(_)) => None,
            Err(err) => panic!("{err}"),
        }
    }

    /// Return the reflect data for a path within the inspected object.
    fn get_reflect<'a>(
        &self,
        world: &'a World,
        path: &ParsedPath,
    ) -> Option<&'a dyn PartialReflect> {
        let cmp = world.get::<T>(self.entity).unwrap();
        match cmp.reflect_path(path) {
            Ok(result) => Some(result),
            Err(ReflectPathError::InvalidAccess(_)) => None,
            Err(err) => panic!("{err}"),
        }
    }

    fn set_value(&self, world: &mut DeferredWorld, path: &ParsedPath, value: &dyn PartialReflect) {
        let mut entt = world.entity_mut(self.entity);
        let mut cmp = entt.get_mut::<T>().unwrap();
        cmp.reflect_path_mut(path).unwrap().apply(value);
    }

    fn update_value(
        &self,
        world: &mut DeferredWorld,
        path: &ParsedPath,
        f: &dyn Fn(&mut dyn PartialReflect),
    ) {
        let mut entt = world.entity_mut(self.entity);
        let mut res = entt.get_mut::<T>().unwrap();
        f(res.reflect_path_mut(path).unwrap());
    }
}

/// A reference to a field within an `InspectableRoot`. This contains information needed to
/// get and set the field as well as query it's type.
#[derive(Clone)]
pub struct Inspectable {
    /// The top-level data structure being inspected, which contains this field.
    pub(crate) root: Arc<dyn InspectableRoot>,
    /// Name of the field.
    pub(crate) name: String,
    /// The path to the struct field or tuple field containing the value. This is used to
    /// add or remove the field from the parent.
    pub(crate) field_path: ParsedPath,
    /// The path to the actual value, which might be wrapped in an `Option` or `Vec`. This is
    /// used to edit the field value.
    pub(crate) value_path: ParsedPath,
    /// If true, then the field can be removed from it's parent.
    pub(crate) can_remove: bool,
    /// If true, then the field can be moved up and down relative to its siblings.
    pub(crate) can_move: bool,
    /// Custom attributes for the field
    pub(crate) attributes: Option<&'static CustomAttributes>,
}

impl Inspectable {
    pub fn from_root(root: Arc<dyn InspectableRoot>) -> Arc<Self> {
        Arc::new(Self {
            root,
            name: "".into(),
            field_path: ParsedPath(Vec::default()),
            value_path: ParsedPath(Vec::default()),
            can_remove: false,
            can_move: false,
            attributes: None,
        })
    }

    /// Return the name of this field.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Get the reflected value of the field.
    pub fn reflect<'a>(&self, world: &'a World) -> Option<&'a dyn PartialReflect> {
        self.root.get_reflect(world, &self.value_path)
    }

    /// Get the reflected value of the field.
    pub fn reflect_tracked<'a>(&self, cx: &'a Cx) -> Option<&'a dyn PartialReflect> {
        self.root.get_reflect_tracked(cx, &self.value_path)
    }

    /// Update the value of the field
    pub fn set_value(&self, world: &mut DeferredWorld, value: &dyn Reflect) {
        self.root.set_value(world, &self.value_path, value);
    }

    /// Whether the item can be removed (in other words, is it optional or an array element)
    pub fn can_remove(&self) -> bool {
        self.can_remove
    }

    /// Use a closure to modify the reflected field data.
    pub fn update_value(&self, world: &mut DeferredWorld, f: &dyn Fn(&mut dyn PartialReflect)) {
        self.root.update_value(world, &self.value_path, f);
    }

    /// Remove the value from the parent. This means different things depending on the type of
    /// parent:
    /// * If the parent is an `Option`, it means setting it to `None`.
    /// * If the parent is an array, it means removing the value from the array.
    pub fn remove(&self, world: &mut DeferredWorld) {
        let Some(field) = self.root.get_reflect(world, &self.field_path) else {
            return;
        };
        match field.get_represented_type_info().unwrap() {
            bevy::reflect::TypeInfo::Struct(_) => todo!(),
            bevy::reflect::TypeInfo::TupleStruct(_) => todo!(),
            bevy::reflect::TypeInfo::Tuple(_) => todo!(),
            bevy::reflect::TypeInfo::List(_) => {
                if let Some(access) = self.value_path.0.last()
                    && let Access::ListIndex(idx) = access.access
                {
                    self.root.update_value(world, &self.field_path, &|list| {
                        if let ReflectMut::List(list_data) = list.reflect_mut() {
                            list_data.remove(idx);
                        } else {
                            panic!("Can't mutate list");
                        }
                    });
                }
                // if let Some(dyn_list) = list.dyn
                // info!("Remove from list");
            }
            bevy::reflect::TypeInfo::Array(_) => todo!(),
            bevy::reflect::TypeInfo::Map(_) => todo!(),
            bevy::reflect::TypeInfo::Set(_) => todo!(),
            bevy::reflect::TypeInfo::Enum(_enum_ref) => {
                if field
                    .reflect_type_path()
                    .starts_with("core::option::Option")
                {
                    let dynamic_enum = DynamicEnum::new("None", DynamicVariant::Unit);
                    self.root.set_value(world, &self.field_path, &dynamic_enum);
                } else {
                    panic!("Can't remove non-optional field");
                }
            }
            bevy::reflect::TypeInfo::Opaque(_) => todo!(),
        }
    }
}
