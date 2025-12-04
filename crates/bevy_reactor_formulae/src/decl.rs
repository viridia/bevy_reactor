// use std::sync::Arc;

use bevy::platform::collections::HashMap;
use smol_str::SmolStr;

use crate::{expr_type::ExprType, location::TokenLocation};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum DeclVisibility {
    Public,
    #[default]
    Private,
}

#[derive(Debug)]
pub struct Decl {
    pub location: TokenLocation,
    // pub name: SmolStr,
    pub visibility: DeclVisibility,
    pub kind: DeclKind,
}

#[derive(Debug)]
pub enum DeclKind {
    Global {
        typ: ExprType,
        is_const: bool,
        /// Index of this variable in the host or module's variables table.
        index: usize,
    },
    Local {
        typ: ExprType,
        is_const: bool,
        /// Index of this variable in the local variables table.
        index: usize,
    },
    Param {
        typ: ExprType,
        /// Index of this variable in the current function's parameter list.
        index: usize,
    },
    Function {
        params: Vec<FunctionParam>,
        ret: ExprType,
        is_native: bool,
        /// Index of this function in the host or module's functions table.
        index: usize,
    },
    TypeAlias(ExprType),
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

    /// Index of this param in the function's local variables. This takes into account multi-value
    /// params.
    pub local_index: usize,
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

#[derive(Debug)]
pub(crate) struct Scope<'parent, 'decls> {
    pub(crate) parent: Option<&'parent Scope<'parent, 'parent>>,
    pub(crate) decls: &'decls DeclTable,
}

impl<'parent, 'decls> Scope<'parent, 'decls> {
    pub fn lookup(&self, name: &str) -> Option<&Decl> {
        if let Some(decl) = self.decls.get(name) {
            return Some(decl);
        }

        if let Some(parent) = self.parent {
            parent.lookup(name)
        } else {
            None
        }
    }
}
