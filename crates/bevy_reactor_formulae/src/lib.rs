pub mod asset;
pub(crate) mod ast;
pub mod compiler;
pub(crate) mod decl;
pub(crate) mod expr;
pub(crate) mod expr_type;
pub mod host;
pub mod instr;
pub mod location;
pub(crate) mod oper;
pub(crate) mod parser;
mod pass;
mod string;
pub mod vm;

pub use compiler::{CompilationError, compile_formula, compile_module};
pub use host::HostState;
pub use vm::{VM, Value};

use crate::{compiler::CompiledFunction, decl::DeclTable};

/// The output of a compilation, either a module or a formula.
/// - a "module" is a standalone asset containing declarations (functions and variables)
/// - a "formula" is a module that is intended to be embedded within another asset. It's similar
///   to a module, however it can have executable statements at the root level which are compiled
///   into a special ".default" function that takes no arguments.
#[derive(Default, Debug)]
pub struct Module {
    /// Name of the asset or file containing the source of this module.
    pub(crate) path: String,

    /// Top-level declarations in this module.
    pub(crate) module_decls: DeclTable,

    /// Compiled functions, including both top-level and nested.
    pub(crate) functions: Vec<CompiledFunction>,
}

impl Module {
    /// The name of the default export.
    pub const DEFAULT: &str = ".default";
}
