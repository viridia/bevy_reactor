use std::sync::Arc;

use bevy::platform::collections::HashMap;
use smol_str::SmolStr;

use crate::{
    VM, Value,
    expr_type::{ExprType, FunctionType},
    location::TokenLocation,
    vm::VMError,
};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum DeclVisibility {
    Public,
    #[default]
    Private,
}

#[derive(Debug)]
pub struct Decl {
    pub location: TokenLocation,
    pub visibility: DeclVisibility,
    pub typ: ExprType,
    pub kind: DeclKind,
}

#[derive(Debug)]
pub enum DeclKind {
    Global {
        is_const: bool,
        /// Index of this variable in the host or module's variables table.
        index: usize,
    },
    Local {
        is_const: bool,
        /// Index of this variable in the local variables table.
        index: usize,
    },
    Param {
        /// Index of this variable in the current function's parameter list.
        index: usize,
    },
    Function {
        /// Index of this function in the host or module's functions table.
        index: usize,
    },
    NativeType {
        /// Index of this type in the host types table.
        index: usize,
    },
    TypeAlias,
}

// #[derive(Debug)]
// #[allow(unused)]
// pub struct ImportDecl {
//     pub location: TokenLocation,
//     pub path: Symbol,
//     pub names: Vec<Symbol>,
// }

/// Declaration for a function parameter
#[derive(Debug, PartialEq, Clone)]
pub struct FunctionParam {
    pub location: TokenLocation,
    pub name: SmolStr,
    pub typ: ExprType,

    /// Index of this param in the params table.
    pub index: usize,
}

// #[derive(Debug)]
// #[allow(unused)]
// pub struct StructDecl {
//     pub location: TokenLocation,
//     pub name: Symbol,
//     pub visibility: DeclVisibility,
//     pub typ: Arc<StructType>,
//     pub index: usize,
// }

// #[derive(Debug, PartialEq, Clone)]
// pub struct FieldDecl {
//     pub location: TokenLocation,
//     pub name: Symbol,
//     pub typ: Type,
//     pub index: usize,
// }

pub type DeclTable = HashMap<SmolStr, Decl>;

/// What kind of scope this is.
#[derive(Debug, Clone, Copy)]
pub enum ScopeType {
    Host,
    Module,
    Import,
    Param,
    Object,
    String,
    Local,
}

#[derive(Debug)]
pub(crate) struct Scope<'parent, 'decls> {
    pub(crate) parent: Option<&'parent Scope<'parent, 'parent>>,
    pub(crate) decls: &'decls DeclTable,
    pub(crate) scope_type: ScopeType,
}

impl<'parent, 'decls> Scope<'parent, 'decls> {
    pub fn lookup(&self, name: &str) -> Option<(ScopeType, &Decl)> {
        if let Some(decl) = self.decls.get(name) {
            return Some((self.scope_type, decl));
        }

        if let Some(parent) = self.parent {
            parent.lookup(name)
        } else {
            None
        }
    }
}

pub type ObjectMethod = fn(&VM, args: &[Value]) -> Result<Value, VMError>;

/// Symbol table containing the methods and fields of a composite type such as a struct.
#[derive(Default, Debug)]
pub(crate) struct ObjectMembers {
    /// Lookup scope for object properties and methods.
    pub(crate) decls: DeclTable,

    /// Properties of entities
    pub(crate) methods: Vec<ObjectMethod>,
}

impl ObjectMembers {
    pub fn get(&self, name: &str) -> Option<&Decl> {
        self.decls.get(name)
    }

    pub fn add_method(
        &mut self,
        name: &'static str,
        method: ObjectMethod,
        params: Vec<FunctionParam>,
        return_type: ExprType,
    ) -> usize {
        let name: SmolStr = name.into();
        if self.decls.contains_key(&name) {
            panic!("Entity method {name} is already defined.");
        }

        let index = self.methods.len();
        self.methods.push(method);
        self.decls.insert(
            name,
            Decl {
                location: TokenLocation::default(),
                visibility: DeclVisibility::Public,
                typ: ExprType::Function(Arc::new(FunctionType {
                    params,
                    ret: return_type,
                })),
                kind: DeclKind::Function { index },
            },
        );

        index
    }
}
