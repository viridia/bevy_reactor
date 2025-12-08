use std::any::TypeId;

use bevy::{platform::collections::HashMap, reflect::Typed};
use smol_str::SmolStr;

use crate::{
    VM, Value,
    decl::{Decl, DeclKind, DeclTable, DeclVisibility},
    expr_type::ExprType,
    location::TokenLocation,
    vm::VMError,
};

#[derive(Default, Debug)]
pub struct SymbolTable<T> {
    by_name: HashMap<&'static str, usize>,
    by_index: Vec<T>,
}

impl<T> SymbolTable<T> {
    pub fn empty() -> Self {
        Self {
            by_name: HashMap::default(),
            by_index: Vec::new(),
        }
    }

    pub fn insert(&mut self, name: &'static str, value: T) -> usize {
        let id = self.by_index.len();
        self.by_name.insert(name, id);
        self.by_index.push(value);
        id
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        self.by_index.get(index)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&'static str, usize)> {
        self.by_name
            .iter()
            .map(move |(&name, &index)| (name, index))
    }
}

type GlobalPropertyAccessor = fn(&VM) -> Result<Value, VMError>;

#[derive(Clone, PartialEq, Debug)]
pub enum Global {
    /// A constant value.
    Const(Value),

    /// A dynamic property, such as `time`.
    Property(GlobalPropertyAccessor),
}

impl Default for Global {
    fn default() -> Self {
        Global::Const(Value::Void)
    }
}

pub type NativePropertyAccessor = fn(&VM, this: Value) -> Result<Value, VMError>;
pub type NativeMethod = fn(&VM, args: &[Value]) -> Result<Value, VMError>;

#[derive(Clone, PartialEq, Debug)]
pub enum NativeMember {
    /// A property, such as `actor.health`
    Property(NativePropertyAccessor),
    /// A method, such as `actor.has_threat()`
    Method(NativeMethod),
    /// A method, such as `actor.has_threat()`
    StaticMethod(NativeMethod),
}

impl Default for NativeMember {
    fn default() -> Self {
        NativeMember::Property(default_native_property)
    }
}

fn default_native_property(_: &VM, _this: Value) -> Result<Value, VMError> {
    panic!("Default property handler")
}

#[derive(Default, Debug)]
pub struct NativeType {
    /// Lookup scope for instance members.
    pub(crate) instance_decls: DeclTable,

    /// Lookup scope for static members.
    pub(crate) static_decls: DeclTable,
}

impl NativeType {
    // pub fn add_method(
    //     &mut self,
    //     name: &'static str,
    //     method: NativeMethod,
    //     result_type: ExprType,
    // ) -> usize {
    //     let name: SmolStr = name.into();
    //     if self.instance_decls.contains_key(&name) {
    //         panic!("Member property {name} is already defined.");
    //     }

    //     let index = self.instance_decls.len();
    //     self.members.push(NativeMember::Method(method));
    //     self.instance_decls.insert(
    //         name,
    //         Decl {
    //             location: TokenLocation::default(),
    //             visibility: DeclVisibility::Public,
    //             typ: ExprType::Function(Arc::new(FunctionType {
    //                 // TODO: fill in
    //                 params: Default::default(),
    //                 // TODO: fill in
    //                 ret: result_type,
    //             })),
    //             kind: DeclKind::Function { index },
    //         },
    //     );

    //     index
    // }

    // pub fn add_static_method(
    //     &mut self,
    //     name: &'static str,
    //     method: NativeStaticMethod,
    //     result_type: ExprType,
    // ) -> usize {
    //     let name: SmolStr = name.into();
    //     if self.static_decls.contains_key(&name) {
    //         panic!("Member property {name} is already defined.");
    //     }

    //     let index = self.static_decls.len();
    //     self.static_methods.push(method);
    //     self.static_decls.insert(
    //         name,
    //         Decl {
    //             location: TokenLocation::default(),
    //             visibility: DeclVisibility::Public,
    //             typ: ExprType::Function(Arc::new(FunctionType {
    //                 // TODO: fill in
    //                 params: Default::default(),
    //                 ret: result_type,
    //             })),
    //             kind: DeclKind::Function { index },
    //         },
    //     );

    //     index
    // }
}

/// Contains global definitions and method registrations for the compiler and VM.
#[derive(Default, Debug)]
pub struct HostState {
    /// Lookup scope for global variables, functions, and properties.
    pub(crate) global_decls: DeclTable,

    /// Table of global variables and properties.
    pub(crate) vars: Vec<Global>,

    /// List of known native types
    pub(crate) native_types: Vec<NativeType>,

    /// Map of type ids to native type indices.
    types_by_id: HashMap<TypeId, usize>,

    /// Members of native types
    pub(crate) native_members: Vec<NativeMember>,
}

impl HostState {
    pub fn new() -> Self {
        let mut this = Self {
            global_decls: DeclTable::default(),
            // entity_decls: DeclTable::default(),
            native_types: Vec::new(),
            types_by_id: HashMap::new(),
            vars: Vec::new(),
            // entity_members: Vec::new(),
            native_members: Vec::new(),
        };

        // Builtin type definitions.
        this.add_type_alias("i32", ExprType::I32);
        this.add_type_alias("i64", ExprType::I64);
        this.add_type_alias("f32", ExprType::F32);
        this.add_type_alias("f64", ExprType::F64);
        this.add_type_alias("bool", ExprType::Boolean);
        this.add_type_alias("String", ExprType::String);
        // this.add_type_alias("Entity", ExprType::Entity);

        this
    }

    pub fn add_type_alias(&mut self, name: impl Into<SmolStr>, typ: ExprType) {
        let name: SmolStr = name.into();
        if self.global_decls.contains_key(&name) {
            panic!("Host symbol {name} is already defined.");
        }
        self.global_decls.insert(
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
        if self.global_decls.contains_key(&name) {
            panic!("Host symbol {name} is already defined.");
        }

        let var_type = value.value_type();
        let index = self.vars.len();
        self.vars.push(Global::Const(value));
        self.global_decls.insert(
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
        if self.global_decls.contains_key(&name) {
            panic!("Host symbol {name} is already defined.");
        }

        let index = self.vars.len();
        self.vars.push(Global::Property(accessor));
        self.global_decls.insert(
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

    // pub fn add_entity_prop(
    //     &mut self,
    //     name: &'static str,
    //     accessor: EntityPropertyAccessor,
    //     typ: ExprType,
    // ) -> usize {
    //     let name: SmolStr = name.into();
    //     if self.entity_decls.contains_key(&name) {
    //         panic!("Entity member {name} is already defined.");
    //     }

    //     let index = self.entity_members.len();
    //     self.entity_members.push(EntityMember::Property(accessor));
    //     self.entity_decls.insert(
    //         name,
    //         Decl {
    //             location: TokenLocation::default(),
    //             visibility: DeclVisibility::Public,
    //             typ,
    //             kind: DeclKind::Global {
    //                 is_const: true,
    //                 index,
    //             },
    //         },
    //     );

    //     index
    // }

    // pub fn add_entity_method(&mut self, name: &'static str, method: EntityMethod) -> usize {
    //     let name: SmolStr = name.into();
    //     if self.entity_decls.contains_key(&name) {
    //         panic!("Entity member {name} is already defined.");
    //     }

    //     let index = self.entity_members.len();
    //     self.entity_members.push(EntityMember::Method(method));
    //     self.entity_decls.insert(
    //         name,
    //         Decl {
    //             location: TokenLocation::default(),
    //             visibility: DeclVisibility::Public,
    //             typ: ExprType::Function(Arc::new(FunctionType {
    //                 // TODO: fill in
    //                 params: Default::default(),
    //                 // TODO: fill in
    //                 ret: ExprType::None,
    //             })),
    //             kind: DeclKind::Function { index },
    //         },
    //     );

    //     index
    // }

    pub fn add_native_type<T: Typed>(&mut self, name: &'static str) {
        let name: SmolStr = name.into();
        if self.global_decls.contains_key(&name) {
            panic!("Host symbol {name} is already defined.");
        }

        let index = self.global_decls.len();
        let type_index = self.native_types.len();
        self.types_by_id.insert(TypeId::of::<T>(), type_index);
        self.native_types.push(NativeType::default());
        self.global_decls.insert(
            name,
            Decl {
                location: TokenLocation::default(),
                visibility: DeclVisibility::Public,
                typ: ExprType::Reflected(T::type_info()),
                kind: DeclKind::NativeType { index },
            },
        );
    }

    pub(crate) fn get_native_type<T: Typed>(&self) -> Option<&NativeType> {
        self.types_by_id
            .get(&TypeId::of::<T>())
            .map(|id| &self.native_types[*id])
    }

    pub(crate) fn get_native_type_mut<T: Typed>(&mut self) -> Option<&mut NativeType> {
        self.types_by_id
            .get(&TypeId::of::<T>())
            .map(|id| &mut self.native_types[*id])
    }

    pub fn add_native_property<T: Typed>(
        &mut self,
        name: impl Into<SmolStr>,
        accessor: NativePropertyAccessor,
        typ: ExprType,
    ) -> usize {
        let name: SmolStr = name.into();
        let native_type_index = self
            .types_by_id
            .get(&TypeId::of::<T>())
            .expect("Attempt to add property to a type that has not been registered");

        let native_type = self.native_types.get_mut(*native_type_index).unwrap();
        if native_type.instance_decls.contains_key(&name) {
            panic!("Member property {name} is already defined.");
        }

        let index = self.native_members.len();
        self.native_members.push(NativeMember::Property(accessor));
        native_type.instance_decls.insert(
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
}
