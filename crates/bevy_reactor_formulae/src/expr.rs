use smol_str::SmolStr;

use crate::{
    expr_type::ExprType,
    location::TokenLocation,
    oper::{BinaryOp, UnaryOp},
};
use core::fmt::Display;

/// Content of an Expression node.
#[derive(Debug)]
pub(crate) enum ExprKind<'e> {
    /// Expression that represents a bare semicolon.
    Empty,
    ConstInteger(i64),
    ConstFloat(f64),
    ConstBool(bool),
    ConstString(SmolStr),
    FunctionRef(usize),
    LocalDecl(usize, Option<&'e mut Expr<'e>>),
    ParamRef(usize),
    LocalRef(usize),
    GlobalRef(usize),
    Field(&'e mut Expr<'e>, usize),
    Index(&'e mut Expr<'e>, usize),
    Call(&'e mut Expr<'e>, Vec<&'e mut Expr<'e>>),
    BinaryExpr {
        op: BinaryOp,
        lhs: &'e mut Expr<'e>,
        rhs: &'e mut Expr<'e>,
    },
    UnaryExpr {
        op: UnaryOp,
        arg: &'e mut Expr<'e>,
    },
    Assign {
        lhs: &'e mut Expr<'e>,
        rhs: &'e mut Expr<'e>,
    },
    AssignOp {
        op: BinaryOp,
        lhs: &'e mut Expr<'e>,
        rhs: &'e mut Expr<'e>,
    },
    Cast(&'e mut Expr<'e>),
    Block(Vec<&'e mut Expr<'e>>, Option<&'e mut Expr<'e>>),
}

/// Expression node.
#[derive(Debug)]
pub(crate) struct Expr<'e> {
    pub(crate) location: TokenLocation,
    pub(crate) kind: ExprKind<'e>,
    pub(crate) typ: ExprType,
}

impl<'e> Display for Expr<'e> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match &self.kind {
            ExprKind::Empty => write!(f, ";"),
            ExprKind::ConstInteger(value) => value.fmt(f),
            ExprKind::ConstFloat(value) => {
                let mut str_val = value.to_string();
                if !str_val.contains(".") {
                    str_val.push_str(".0");
                }
                str_val.fmt(f)
            }
            ExprKind::ConstBool(value) => value.fmt(f),
            ExprKind::ConstString(symbol) => write!(f, "String({})", symbol),
            ExprKind::FunctionRef(id) => write!(f, "Function({})", id),
            ExprKind::LocalRef(id) => write!(f, "LocalRef({})", id),
            ExprKind::GlobalRef(id) => write!(f, "GlobalRef({})", id),
            ExprKind::Field(base, index) => {
                base.fmt(f)?;
                write!(f, ".{}", index)
            }
            ExprKind::Index(base, index) => {
                base.fmt(f)?;
                write!(f, ".{}", index)
            }
            ExprKind::ParamRef(id) => write!(f, "ParamRef({})", id),
            ExprKind::LocalDecl(id, init) => {
                write!(f, "Local({}", id)?;
                if let Some(init) = init {
                    write!(f, " = ")?;
                    init.fmt(f)?;
                }
                write!(f, ")")
            }
            ExprKind::BinaryExpr { op, lhs, rhs } => {
                // TODO: Parens if necessary.
                lhs.fmt(f)?;
                write!(f, " {} ", op)?;
                rhs.fmt(f)
            }
            ExprKind::UnaryExpr { op, arg } => {
                write!(f, "{:?}", op)?;
                arg.fmt(f)
            }
            ExprKind::Assign { lhs, rhs } => {
                lhs.fmt(f)?;
                write!(f, " = ")?;
                rhs.fmt(f)
            }
            ExprKind::AssignOp { op, lhs, rhs } => {
                lhs.fmt(f)?;
                write!(f, " {:?}= ", op)?;
                rhs.fmt(f)
            }
            ExprKind::Cast(arg) => {
                arg.fmt(f)?;
                write!(f, " as ")?;
                self.typ.fmt(f)
            }
            ExprKind::Call(func, args) => {
                func.fmt(f)?;
                write!(f, "(")?;
                for arg in args {
                    arg.fmt(f)?;
                    write!(f, ", ")?;
                }
                write!(f, ")")
            }
            ExprKind::Block(stmts, result) => {
                write!(f, "{{")?;
                for stmt in stmts {
                    stmt.fmt(f)?;
                }
                if let Some(result) = result {
                    write!(f, "; ")?;
                    result.fmt(f)?;
                }
                write!(f, "}}")
            }
        }
    }
}

impl<'e> Expr<'e> {
    pub fn new(location: TokenLocation, value: ExprKind<'e>) -> Self {
        Self {
            location,
            kind: value,
            typ: ExprType::None,
        }
    }

    pub fn with_type(&mut self, typ: ExprType) -> &mut Self {
        self.typ = typ;
        self
    }
}
