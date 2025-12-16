use std::{any::TypeId, sync::Arc};

use bevy::{ecs::entity::Entity, platform::collections::HashMap, reflect::Typed};
use smol_str::SmolStr;

use crate::{
    VM,
    decl::{Decl, DeclKind, DeclTable, DeclVisibility, FunctionParam},
    expr_type::{ExprType, FunctionType, Param},
    location::TokenLocation,
    string::init_string_methods,
    vm::{InvocationContext, StackValue, VMError},
};

pub type HostInstanceProperty =
    fn(&mut InvocationContext, this: StackValue) -> Result<StackValue, VMError>;
pub type HostFunction = fn(&mut InvocationContext) -> Result<StackValue, VMError>;

/// Symbol table containing the methods and fields of a composite type such as a struct.
#[derive(Default, Debug)]
pub struct HostType {
    /// Lookup scope for object properties and methods.
    pub(crate) decls: DeclTable,
}

impl HostType {
    pub fn get(&self, name: &str) -> Option<&Decl> {
        self.decls.get(name)
    }
}

pub struct HostTypeBuilder<'host> {
    decls: &'host mut DeclTable,
    members: &'host mut Vec<Global>,
}

impl<'host> HostTypeBuilder<'host> {
    /// Add a property to this type.
    pub fn add_property(
        &mut self,
        name: impl Into<SmolStr>,
        accessor: HostInstanceProperty,
        typ: ExprType,
    ) -> &mut Self {
        let name: SmolStr = name.into();
        if self.decls.contains_key(&name) {
            panic!("Object member {name} is already defined.");
        }

        let index = self.members.len();
        self.members.push(Global::InstanceProperty(accessor));
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
        self.members.push(Global::InstanceMethod(method));
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

    /// Add a native method to this type.
    pub fn add_static_method(
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
        self.members.push(Global::Function(method));
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

type GlobalPropertyAccessor = fn(&VM) -> Result<StackValue, VMError>;

#[derive(Clone, PartialEq, Debug)]
pub enum Global {
    /// A constant value.
    Const(StackValue),

    /// A dynamic property, such as `time`.
    GlobalProperty(GlobalPropertyAccessor),

    /// A callable function which does not require a `self` argument.
    Function(HostFunction),

    /// A property of a type
    InstanceProperty(HostInstanceProperty),

    /// A method of a type
    InstanceMethod(HostFunction),
}

impl Default for Global {
    fn default() -> Self {
        Global::Const(StackValue::Void)
    }
}

/// Contains global definitions and method registrations for the compiler and VM.
#[derive(Default, Debug)]
pub struct HostState {
    /// Lookup scope for global variables, functions, and properties. This contains all
    /// host symbols that can be looked up without a scope.
    pub(crate) decls: DeclTable,

    /// Table of global variables, properties, and functions.
    pub(crate) members: Vec<Global>,

    /// List of known native types
    pub(crate) types: HashMap<TypeId, HostType>,
}

impl HostState {
    pub fn new() -> Self {
        let mut this = Self {
            decls: DeclTable::default(),
            types: HashMap::new(),
            members: Vec::new(),
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

    pub fn add_global_const(&mut self, name: impl Into<SmolStr>, value: StackValue) -> usize {
        let name: SmolStr = name.into();
        if self.decls.contains_key(&name) {
            panic!("Host symbol {name} is already defined.");
        }

        let var_type = match value {
            StackValue::Bool(_) => ExprType::Boolean,
            StackValue::I32(_) => ExprType::I32,
            StackValue::I64(_) => ExprType::I64,
            StackValue::F32(_) => ExprType::F32,
            StackValue::F64(_) => ExprType::F64,
            StackValue::Entity(_) => ExprType::Entity,
            StackValue::String(_) => ExprType::String,
            _ => {
                panic!("Unsupported constant type");
            }
        };
        let index = self.members.len();
        self.members.push(Global::Const(value));
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

        let index = self.members.len();
        self.members.push(Global::GlobalProperty(accessor));
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

    pub fn add_host_type<T: Typed>(&mut self, name: &'static str) -> HostTypeBuilder {
        let name: SmolStr = name.into();
        if self.decls.contains_key(&name) {
            panic!("Host symbol {name} is already defined.");
        }

        let id = TypeId::of::<T>();
        self.types.entry(id).insert(HostType::default());
        self.decls.insert(
            name,
            Decl {
                location: TokenLocation::default(),
                visibility: DeclVisibility::Public,
                typ: ExprType::Reflected(T::type_info()),
                kind: DeclKind::NativeType { id },
            },
        );

        let host_type = self.types.get_mut(&id).unwrap();
        HostTypeBuilder {
            decls: &mut host_type.decls,
            members: &mut self.members,
        }
    }

    /// Returns the `HostType` table for a native type.
    pub fn get_host_type<T: Typed>(&self) -> Option<&HostType> {
        self.types.get(&TypeId::of::<T>())
    }

    /// Returns the mutable `HostType` table for a native type.
    pub fn get_host_type_mut<T: Typed>(&mut self) -> Option<HostTypeBuilder> {
        self.types
            .get_mut(&TypeId::of::<T>())
            .map(|host_type| HostTypeBuilder {
                decls: &mut host_type.decls,
                members: &mut self.members,
            })
    }

    /// Returns the `HostType` table for a native type.
    pub fn get_host_type_by_id(&self, id: TypeId) -> Option<&HostType> {
        self.types.get(&id)
    }
}
