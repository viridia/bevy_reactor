use std::{any::TypeId, sync::Arc};

use bevy::{ecs::entity::Entity, platform::collections::HashMap, reflect::Typed};
use smol_str::SmolStr;

use crate::{
    VM, Value,
    decl::{Decl, DeclKind, DeclTable, DeclVisibility, FunctionParam},
    expr_type::{ExprType, FunctionType, Param},
    location::TokenLocation,
    string::init_string_methods,
    vm::{CallContext, VMError},
};

pub type HostTypeProperty = fn(&VM, this: Value) -> Result<Value, VMError>;
// pub type HostTypeMethod = fn(&VM, args: &[Value]) -> Result<Value, VMError>;
pub type HostFunction = fn(&mut CallContext) -> Result<Value, VMError>;

#[derive(Clone, PartialEq, Debug)]
pub enum HostTypeMember {
    /// A property, such as `actor.health`
    Property(HostTypeProperty),
    /// A method, such as `actor.has_threat()`
    Method(HostFunction),
    /// A method, such as `actor.has_threat()`
    StaticMethod(HostFunction),
}

/// Symbol table containing the methods and fields of a composite type such as a struct.
#[derive(Default, Debug)]
pub struct HostType {
    /// Lookup scope for object properties and methods.
    pub(crate) decls: DeclTable,

    /// Properties of entities
    pub(crate) members: Vec<HostTypeMember>,
}

impl HostType {
    pub fn get(&self, name: &str) -> Option<&Decl> {
        self.decls.get(name)
    }

    /// Add a property to this type.
    pub fn add_property(
        &mut self,
        name: impl Into<SmolStr>,
        accessor: HostTypeProperty,
        typ: ExprType,
    ) -> &mut Self {
        let name: SmolStr = name.into();
        if self.decls.contains_key(&name) {
            panic!("Object member {name} is already defined.");
        }

        let index = self.members.len();
        self.members.push(HostTypeMember::Property(accessor));
        self.decls.insert(
            name,
            Decl {
                location: TokenLocation::default(),
                visibility: DeclVisibility::Public,
                typ,
                kind: DeclKind::Global {
                    is_const: true,
                    index,
                },
            },
        );

        self
    }

    /// Add a native method to this type.
    pub fn add_method(
        &mut self,
        name: &'static str,
        method: HostFunction,
        params: Vec<Param>,
        return_type: ExprType,
    ) -> &mut Self {
        let name: SmolStr = name.into();
        if self.decls.contains_key(&name) {
            panic!("Object member {name} is already defined.");
        }

        let index = self.members.len();
        self.members.push(HostTypeMember::Method(method));
        self.decls.insert(
            name,
            Decl {
                location: TokenLocation::default(),
                visibility: DeclVisibility::Public,
                typ: ExprType::Function(Arc::new(FunctionType {
                    params: params
                        .iter()
                        .enumerate()
                        .map(|(index, p)| FunctionParam {
                            location: TokenLocation::default(),
                            name: p.name.clone(),
                            typ: p.ty.clone(),
                            index,
                        })
                        .collect(),
                    ret: return_type,
                })),
                kind: DeclKind::Function { index },
            },
        );

        self
    }
}

type GlobalPropertyAccessor = fn(&VM) -> Result<Value, VMError>;

#[derive(Clone, PartialEq, Debug)]
pub enum Global {
    /// A constant value.
    Const(Value),

    /// A dynamic property, such as `time`.
    Property(GlobalPropertyAccessor),

    /// A callable function which does not require a `self` argument.
    Function(HostFunction),
}

impl Default for Global {
    fn default() -> Self {
        Global::Const(Value::Void)
    }
}

/// Contains global definitions and method registrations for the compiler and VM.
#[derive(Default, Debug)]
pub struct HostState {
    /// Lookup scope for global variables, functions, and properties. This contains all
    /// host symbols that can be looked up without a scope.
    pub(crate) decls: DeclTable,

    /// Table of global variables, properties, and functions.
    pub(crate) globals: Vec<Global>,

    /// List of known native types
    pub(crate) host_types: HashMap<TypeId, HostType>,
}

impl HostState {
    pub fn new() -> Self {
        let mut this = Self {
            decls: DeclTable::default(),
            host_types: HashMap::new(),
            globals: Vec::new(),
        };

        // Builtin primitive type definitions.
        this.add_type_alias("i32", ExprType::I32);
        this.add_type_alias("i64", ExprType::I64);
        this.add_type_alias("f32", ExprType::F32);
        this.add_type_alias("f64", ExprType::F64);
        this.add_type_alias("bool", ExprType::Boolean);

        this.add_host_type::<Entity>("Entity");
        this.add_host_type::<String>("String");

        init_string_methods(&mut this);
        this
    }

    pub fn add_type_alias(&mut self, name: impl Into<SmolStr>, typ: ExprType) {
        let name: SmolStr = name.into();
        if self.decls.contains_key(&name) {
            panic!("Host symbol {name} is already defined.");
        }
        self.decls.insert(
            name,
            Decl {
                location: TokenLocation::default(),
                visibility: DeclVisibility::Public,
                typ,
                kind: DeclKind::TypeAlias,
            },
        );
    }

    pub fn add_global_const(&mut self, name: impl Into<SmolStr>, value: Value) -> usize {
        let name: SmolStr = name.into();
        if self.decls.contains_key(&name) {
            panic!("Host symbol {name} is already defined.");
        }

        let var_type = match value {
            Value::Bool(_) => ExprType::Boolean,
            Value::I32(_) => ExprType::I32,
            Value::I64(_) => ExprType::I64,
            Value::F32(_) => ExprType::F32,
            Value::F64(_) => ExprType::F64,
            Value::Entity(_) => ExprType::Entity,
            Value::String(_) => ExprType::String,
            _ => {
                panic!("Unsupported constant type");
            }
        };
        let index = self.globals.len();
        self.globals.push(Global::Const(value));
        self.decls.insert(
            name,
            Decl {
                location: TokenLocation::default(),
                visibility: DeclVisibility::Public,
                typ: var_type,
                kind: DeclKind::Global {
                    is_const: true,
                    index,
                },
            },
        );

        index
    }

    pub fn add_global_prop(
        &mut self,
        name: impl Into<SmolStr>,
        accessor: GlobalPropertyAccessor,
        typ: ExprType,
    ) -> usize {
        let name: SmolStr = name.into();
        if self.decls.contains_key(&name) {
            panic!("Host symbol {name} is already defined.");
        }

        let index = self.globals.len();
        self.globals.push(Global::Property(accessor));
        self.decls.insert(
            name,
            Decl {
                location: TokenLocation::default(),
                visibility: DeclVisibility::Public,
                typ,
                kind: DeclKind::Global {
                    is_const: true,
                    index,
                },
            },
        );

        index
    }

    pub fn add_host_type<T: Typed>(&mut self, name: &'static str) -> &mut HostType {
        let name: SmolStr = name.into();
        if self.decls.contains_key(&name) {
            panic!("Host symbol {name} is already defined.");
        }

        let id = TypeId::of::<T>();
        self.host_types.entry(id).insert(HostType::default());
        self.decls.insert(
            name,
            Decl {
                location: TokenLocation::default(),
                visibility: DeclVisibility::Public,
                typ: ExprType::Reflected(T::type_info()),
                kind: DeclKind::NativeType { id },
            },
        );

        self.host_types.get_mut(&id).unwrap()
    }

    /// Add a static method to a native type.
    pub fn add_static_method<T: Typed>(
        &mut self,
        name: &'static str,
        method: HostFunction,
        params: Vec<Param>,
        return_type: ExprType,
    ) -> &mut Self {
        let name: SmolStr = name.into();
        let index = self.globals.len();
        let Some(host_type) = self.get_host_type_mut::<T>() else {
            panic!(
                "Host type {} must be registered first",
                T::type_info().type_path()
            );
        };

        if host_type.decls.contains_key(&name) {
            panic!("Object member {name} is already defined.");
        }

        host_type.decls.insert(
            name,
            Decl {
                location: TokenLocation::default(),
                visibility: DeclVisibility::Public,
                typ: ExprType::Function(Arc::new(FunctionType {
                    params: params
                        .iter()
                        .enumerate()
                        .map(|(index, p)| FunctionParam {
                            location: TokenLocation::default(),
                            name: p.name.clone(),
                            typ: p.ty.clone(),
                            index,
                        })
                        .collect(),
                    ret: return_type,
                })),
                kind: DeclKind::Function { index },
            },
        );
        self.globals.push(Global::Function(method));

        self
    }

    /// Returns the `HostType` table for a native type.
    pub fn get_host_type<T: Typed>(&self) -> Option<&HostType> {
        self.host_types.get(&TypeId::of::<T>())
    }

    /// Returns the mutable `HostType` table for a native type.
    pub fn get_host_type_mut<T: Typed>(&mut self) -> Option<&mut HostType> {
        self.host_types.get_mut(&TypeId::of::<T>())
    }

    /// Returns the `HostType` table for a native type.
    pub fn get_host_type_by_id(&self, id: TypeId) -> Option<&HostType> {
        self.host_types.get(&id)
    }
}
