use smol_str::SmolStr;

use crate::oper::{BinaryOp, UnaryOp};

#[derive(Debug, Default)]
pub(crate) enum BlockTerminator {
    #[default]
    /// Termination hasn't been specified yet
    Invalid,
    /// Return from function
    Return,
    /// Unconditional transfer to another block
    Jump { target: usize },
    /// Conditional branch to one of two blocks
    Branch {
        // condition: &'e mut Expr<'e>,
        then_target: usize,
        else_target: usize,
    },
}

/// Basic block for control flow.
#[derive(Debug, Default)]
pub(crate) struct BasicBlock {
    /// Index of this block
    pub(crate) id: usize,
    /// Entry point of this block within the generated code buffer.
    pub(crate) start_offset: usize,
    /// List of instructions within the body of this block.
    pub(crate) ops: Vec<StackOp>,
    /// Block terminator
    pub(crate) term: BlockTerminator,
    /// Blocks leading to this block
    pub(crate) predecessors: Vec<usize>,
    // pub(crate) successors: Vec<usize>,
}

#[derive(Debug, Default, PartialEq)]
pub(crate) enum StackOp {
    #[default]
    ConstVoid,
    ConstBool(bool),
    ConstI32(i32),
    ConstI64(i64),
    ConstF32(f32),
    ConstF64(f64),
    ConstStr(SmolStr),
    Drop(usize),
    LoadParam(u16),
    LoadLocal(u16),
    LoadGlobal(u16),
    LoadNativeProp(u16),
    LoadField(u16),
    StoreLocal(u16),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    CallHostMethod {
        method: u16,
        num_args: u16,
    },
    CallHostFunction {
        method: u16,
        num_args: u16,
    },
    CallModuleFunction {
        method: u16,
        num_args: u16,
    },
    Return,
}
