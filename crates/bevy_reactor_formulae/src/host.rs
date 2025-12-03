use bevy::{ecs::entity::Entity, platform::collections::HashMap};
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

pub type EntityPropertyAccessor = fn(&VM, e: Entity) -> Result<Value, VMError>;
pub type EntityMethod = fn(&VM, e: Entity, args: &[Value]) -> Result<Value, VMError>;

#[derive(Clone, PartialEq, Debug)]
pub enum EntityMember {
    /// A property, such as `actor.health`
    Property(EntityPropertyAccessor),
    /// A method, such as `actor.has_threat()`
    Method(EntityMethod),
}

impl Default for EntityMember {
    fn default() -> Self {
        EntityMember::Property(default_property)
    }
}

fn default_property(_: &VM, e: Entity) -> Result<Value, VMError> {
    panic!("Default property handler")
}

/// Contains global definitions and method registrations for the compiler and VM.
#[derive(Default, Debug)]
pub struct HostState {
    /// Lookup scope for global variables, functions, and properties.
    pub(crate) global_decls: DeclTable,

    /// Lookup scope for entity members.
    pub(crate) entity_decls: DeclTable,

    /// Table of global variables and properties.
    pub(crate) vars: Vec<Global>,

    /// Properties of entities
    // pub entity: SymbolTable<EntityMember>,
    pub(crate) entity_members: Vec<EntityMember>,
}

impl HostState {
    pub fn new() -> Self {
        let mut this = Self {
            global_decls: DeclTable::default(),
            entity_decls: DeclTable::default(),
            vars: Vec::new(),
            entity_members: Vec::new(),
            // global: SymbolTable::default(),
            // entity: SymbolTable::default(),
        };

        // Builtin type definitions.
        this.add_type_alias("i32", ExprType::I32);
        this.add_type_alias("i64", ExprType::I64);
        this.add_type_alias("f32", ExprType::F32);
        this.add_type_alias("f64", ExprType::F64);
        this.add_type_alias("bool", ExprType::Boolean);
        this.add_type_alias("String", ExprType::String);
        this.add_type_alias("Entity", ExprType::Entity);

        // for (name, index) in host.global.iter() {
        //     let sym = symbols.intern(name);
        //     // Host names cannot override builtin names
        //     if intrinsic_scope.contains(sym) {
        //         panic!("The host name {name} is already defined.");
        //     }
        //     let value = host.global.get(index).unwrap();
        //     match value {
        //         crate::host::Global::Const(value) => {
        //             intrinsic_scope.insert(sym, Decl::Global(value.value_type(), index));
        //         }
        //         crate::host::Global::Property { accessor: _, typ } => {
        //             intrinsic_scope.insert(sym, Decl::Global(typ.clone(), index));
        //         }
        //     }
        // }
        this
    }

    pub fn add_type_alias(&mut self, name: impl Into<SmolStr>, typ: ExprType) {
        let name: SmolStr = name.into();
        if self.global_decls.contains_key(&name) {
            panic!("Host symbol {name} is already defined.");
        }
        self.global_decls.insert(
            name.clone(),
            Decl {
                location: TokenLocation::default(),
                name,
                visibility: DeclVisibility::Public,
                kind: DeclKind::TypeAlias(typ),
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
            name.clone(),
            Decl {
                location: TokenLocation::default(),
                name,
                visibility: DeclVisibility::Public,
                kind: DeclKind::Global {
                    typ: var_type,
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
            name.clone(),
            Decl {
                location: TokenLocation::default(),
                name,
                visibility: DeclVisibility::Public,
                kind: DeclKind::Global {
                    typ,
                    is_const: true,
                    index,
                },
            },
        );

        index
    }

    pub fn add_entity_prop(
        &mut self,
        name: &'static str,
        accessor: EntityPropertyAccessor,
        typ: ExprType,
    ) -> usize {
        let name: SmolStr = name.into();
        if self.entity_decls.contains_key(&name) {
            panic!("Entity member {name} is already defined.");
        }

        let index = self.entity_members.len();
        self.entity_members.push(EntityMember::Property(accessor));
        self.entity_decls.insert(
            name.clone(),
            Decl {
                location: TokenLocation::default(),
                name,
                visibility: DeclVisibility::Public,
                kind: DeclKind::Global {
                    typ,
                    is_const: true,
                    index,
                },
            },
        );

        index
    }

    // pub fn add_entity_method(&mut self, name: &'static str, method: EntityMethod) -> usize {
    //     let name: SmolStr = name.into();
    //     if self.entity_decls.contains_key(&name) {
    //         panic!("Entity member {name} is already defined.");
    //     }

    //     let index = self.entity_members.len();
    //     self.entity_members.push(EntityMember::Method(method));
    //     self.entity_decls.insert(
    //         name.clone(),
    //         Decl {
    //             location: TokenLocation::default(),
    //             name,
    //             visibility: DeclVisibility::Public,
    //             kind: DeclKind::Global {
    //                 typ,
    //                 is_const: true,
    //                 index,
    //             },
    //         },
    //     );

    //     index
    // }
}
