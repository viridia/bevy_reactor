use smol_str::SmolStr;

use crate::{
    decl::DeclVisibility,
    location::TokenLocation,
    oper::{BinaryOp, UnaryOp},
};

/// AST node.
#[derive(Debug)]
pub(crate) struct ASTNode<'a> {
    pub(crate) location: TokenLocation,
    pub(crate) kind: NodeKind<'a>,
}

impl<'a> ASTNode<'a> {
    pub fn new(location: impl Into<TokenLocation>, kind: NodeKind<'a>) -> Self {
        Self {
            location: location.into(),
            kind,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum IntegerSuffix {
    Unsized,
    I32,
    I64,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum FloatSuffix {
    F32,
    F64,
}

// Content of an AST node.
#[derive(Debug)]
pub(crate) enum NodeKind<'a> {
    Module(&'a [&'a ASTNode<'a>]),
    Empty,
    // Import(&'a ASTNode<'a>, &'a [Symbol]),
    Decl(&'a ASTDecl<'a>),
    Block(&'a [&'a ASTNode<'a>], Option<&'a ASTNode<'a>>),
    LitBool(bool),
    LitInt(i64, IntegerSuffix),
    LitFloat(f64, FloatSuffix),
    LitString(SmolStr),
    Ident(SmolStr),
    QName(&'a [&'a ASTNode<'a>]),
    Call(&'a ASTNode<'a>, &'a [&'a ASTNode<'a>]),
    FieldName(&'a ASTNode<'a>, SmolStr),
    FieldIndex(&'a ASTNode<'a>, usize),
    Cast {
        arg: &'a ASTNode<'a>,
        typ: &'a ASTNode<'a>,
    },
    UnaryExpr {
        op: UnaryOp,
        arg: &'a ASTNode<'a>,
    },
    BinaryExpr {
        op: BinaryOp,
        lhs: &'a ASTNode<'a>,
        rhs: &'a ASTNode<'a>,
    },
    Assign {
        lhs: &'a ASTNode<'a>,
        rhs: &'a ASTNode<'a>,
    },
    AssignOp {
        op: BinaryOp,
        lhs: &'a ASTNode<'a>,
        rhs: &'a ASTNode<'a>,
    },
    ArrayType(&'a ASTNode<'a>),
    If {
        condition: &'a ASTNode<'a>,
        then_block: &'a ASTNode<'a>,
        else_block: Option<&'a ASTNode<'a>>,
    },
    // StructType
    // TupleStructType
    // EnumType
    // FunctionType
    // TypeAlias
}

// Content of an AST declaration.
#[derive(Debug)]
pub(crate) enum ASTDecl<'ast> {
    Function {
        name: SmolStr,
        visibility: DeclVisibility,
        params: &'ast [&'ast ASTFunctionParam<'ast>],
        ret: Option<&'ast ASTNode<'ast>>,
        body: Option<&'ast ASTNode<'ast>>,
    },
    Let {
        name: SmolStr,
        visibility: DeclVisibility,
        is_const: bool,
        typ: Option<&'ast ASTNode<'ast>>,
        value: Option<&'ast ASTNode<'ast>>,
    },
    Struct {
        name: SmolStr,
        visibility: DeclVisibility,
        fields: &'ast [&'ast ASTStructField<'ast>],
    },
    // TupleStruct
    // TypeAlias {
    //     name: Symbol,
    //     visibility: DeclVisibility,
    //     typ: &'a ASTNode<'a>,
    // },
}

/// AST for a function parameter declaration
#[derive(Debug)]
pub(crate) struct ASTFunctionParam<'a> {
    pub(crate) location: TokenLocation,
    pub(crate) name: SmolStr,
    pub(crate) typ: &'a ASTNode<'a>,
}

/// AST for a structure field declaration
#[derive(Debug)]
pub(crate) struct ASTStructField<'a> {
    pub(crate) location: TokenLocation,
    pub(crate) name: SmolStr,
    pub(crate) typ: &'a ASTNode<'a>,
}
