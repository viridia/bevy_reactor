// use std::sync::Arc;

use bevy::platform::collections::HashMap;
use smol_str::SmolStr;

use crate::{expr_type::ExprType, location::TokenLocation};

// #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
// pub struct Symbol(pub(crate) usize);

// impl Default for Symbol {
//     fn default() -> Self {
//         Self(usize::MAX)
//     }
// }

// #[derive(Debug)]
// struct SymbolTableInner {
//     symbols: HashMap<String, Symbol>,
//     strings: Vec<String>,
// }

// #[derive(Debug)]
// pub(crate) struct InternedSymbols(RefCell<SymbolTableInner>);

// impl InternedSymbols {
//     pub fn new() -> Self {
//         Self(RefCell::new(SymbolTableInner {
//             symbols: HashMap::new(),
//             strings: Vec::new(),
//         }))
//     }

//     pub fn intern(&self, name: &str) -> Symbol {
//         let mut inner = self.0.borrow_mut();
//         match inner.symbols.get(name) {
//             Some(symbol) => *symbol,
//             None => {
//                 let id = inner.strings.len();
//                 inner.strings.push(name.to_string());
//                 let symbol = Symbol(id);
//                 inner.symbols.insert(name.to_string(), symbol);
//                 symbol
//             }
//         }
//     }

//     pub fn resolve(&self, symbol: Symbol) -> String {
//         let inner = self.0.borrow();
//         inner.strings[symbol.0].clone()
//     }
// }

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum DeclVisibility {
    Public,
    #[default]
    Private,
}

// #[derive(Debug)]
// pub enum Decl {
//     Local(usize),
//     Global(ExprType, usize),
//     Param(ExprType, usize),
//     Function(usize),
//     Struct(usize),
//     // Enum(usize),
//     TypeAlias(ExprType),
// }

#[derive(Debug)]
pub struct Decl {
    pub location: TokenLocation,
    pub name: SmolStr,
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
        params: Vec<ParamDecl>,
        ret: ExprType,
        // body: Option<&'e Expr<'e>>,
        // locals: Vec<LocalDecl>,
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

// #[derive(Debug)]
// pub struct FunctionDecl<'e> {
//     pub location: TokenLocation,
//     pub name: Symbol,
//     pub visibility: DeclVisibility,
//     pub typ: Arc<FunctionType>,
//     pub body: Option<&'e Expr<'e>>,
//     pub locals: Vec<LocalDecl>,
//     pub is_native: bool,
//     /// Index of this function in the module's functions table.
//     pub function_index: usize,
// }

/// Declaration for a local variable or constant
// #[derive(Debug, Clone)]
// #[allow(unused)]
// pub struct LocalDecl {
//     pub location: TokenLocation,
//     pub visibility: DeclVisibility,
//     pub name: Symbol,
//     pub typ: ExprType,
//     pub index: usize,
//     /// Index of this param in the function's local variables. This takes into account multi-value
//     /// params.
//     pub local_index: usize,
//     pub is_const: bool,
// }

// / Declaration for a global variable or constant
// #[derive(Debug, Clone)]
// #[allow(unused)]
// pub struct GlobalDecl {
//     pub location: TokenLocation,
//     pub visibility: DeclVisibility,
//     pub name: Symbol,
//     pub typ: Type,
//     pub index: usize,
//     pub is_const: bool,
// }

/// Declaration for a function parameter
#[derive(Debug, PartialEq, Clone)]
pub struct ParamDecl {
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

// /// Declaration for a function parameter
// #[derive(Debug, PartialEq, Clone)]
// pub struct FieldDecl {
//     pub location: TokenLocation,
//     pub name: Symbol,
//     pub typ: Type,
//     pub index: usize,
// }

// #[derive(Debug)]
// pub struct Decls<'e> {
//     pub symbols: InternedSymbols,
//     // pub imports: Vec<ImportDecl>,
//     // pub structs: Vec<StructDecl>,
//     // pub globals: Vec<GlobalDecl>,
//     pub functions: Vec<FunctionDecl<'e>>,
// }

// impl<'e> Decls<'e> {
//     pub fn new() -> Self {
//         Self {
//             symbols: InternedSymbols::new(),
//             // imports: Vec::new(),
//             // structs: Vec::new(),
//             // globals: Vec::new(),
//             functions: Vec::new(),
//         }
//     }

//     pub fn add_function(&mut self, decl: FunctionDecl<'e>) -> usize {
//         let index = self.functions.len();
//         self.functions.push(decl);
//         index
//     }
// }

pub type DeclTable = HashMap<SmolStr, Decl>;

#[derive(Debug)]
pub(crate) struct Scope<'parent, 'decls> {
    pub(crate) parent: Option<&'parent Scope<'parent, 'parent>>,
    pub(crate) decls: &'decls DeclTable,
}

impl<'parent, 'decls> Scope<'parent, 'decls> {
    // pub fn new(parent: Option<&'parent Scope>) -> Self {
    //     Self {
    //         parent,
    //         decls: HashMap::new(),
    //     }
    // }

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

    pub fn get(&self, name: &str) -> Option<&Decl> {
        self.decls.get(name)
    }

    // pub fn insert(&mut self, name: SmolStr, decl: Decl2) {
    //     self.decls.insert(name, decl);
    // }

    pub fn contains(&self, symbol: &str) -> bool {
        self.decls.contains_key(symbol)
    }
}
