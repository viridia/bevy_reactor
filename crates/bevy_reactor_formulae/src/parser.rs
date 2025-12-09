use crate::ast::{
    ASTDecl, ASTFunctionParam, ASTNode, ASTStructField, FloatSuffix, IntegerSuffix, NodeKind,
};
use crate::decl;
use crate::oper::{BinaryOp, UnaryOp};
use bumpalo::Bump;
use smol_str::SmolStr;

peg::parser! {
    pub grammar formula_parser<'a, 's>(arena: &'a Bump) for str {
        rule ws() = quiet!{[' ' | '\n' | '\t']}
        rule line_comment() = quiet!{("//" [^'\n']*)}
        rule _() = quiet!{(ws() / line_comment())*}

        rule digits() = n:$(['0'..='9']['0'..='9' | '_']*) { }
        rule name() -> SmolStr =
            n:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*)
            {
                SmolStr::new(n)
            }

        rule lit_float() -> &'a ASTNode<'a> =
            start:position!()
            n:$(
                digits() &(['.' | 'e' | 'f'])
                ("." ['0'..='9' | '_']*)?
                ("e" ['+' | '-']? digits())?
            )
            s:("f32" { FloatSuffix::F32 } / "f64" { FloatSuffix::F64 })?
            end:position!()
        {?
            let value = n.parse::<f64>().map_err(|_| "invalid float literal")?;
            Ok(arena.alloc(ASTNode::new((start, end), NodeKind::LitFloat(value, s.unwrap_or(FloatSuffix::F32)))))
        }

        rule lit_int() -> &'a ASTNode<'a> =
            start:position!()
            n:$digits()
            s:("i32" { IntegerSuffix::I32 } / "i64" { IntegerSuffix::I64 })?
            end:position!()
        {?
            let value = n.parse::<i64>().map_err(|_| "invalid integer literal")?;
            Ok(arena.alloc(ASTNode::new((start, end), NodeKind::LitInt(value, s.unwrap_or(IntegerSuffix::Unsized)))))
        }

        rule index() -> i64 = n:$digits()
        {?
            n.parse::<i64>().map_err(|_| "invalid integer literal")
        }

        rule lit_string() -> &'a ASTNode<'a> =
            start:position!()
            ['"']
            s:(
                (['\\'] ch:(
                    ['n'] { '\n' }
                    / ['r'] { '\r' }
                    / ['t'] { '\t' }
                    / ['"'] { '\"' }
                    / ['\''] { '\'' }
                    / ['\\'] { '\\' }
                    / ['0'] { '\0' }
                    / ['u'] ['{'] d:$(['0'..='9' | 'a'..='f' | 'A'..='F']*<1,6>) ['}'] {
                        let code = u32::from_str_radix(d, 16).unwrap();
                        core::char::from_u32(code).unwrap()
                    }
                ) { ch })
                / [^'"' | '\\']
                / expected!("string char")
            )*
            ['"']
            end:position!()
        {
            let mut value = String::new();
            for part in s {
                value.push(part);
            }
            arena.alloc(ASTNode::new((start, end), NodeKind::LitString(SmolStr::new(&value))))
        }

        rule lit_bool() -> &'a ASTNode<'a> =
            start:position!()
            value:( "true" { true } / "false" { false } )
            end:position!()
        {
            arena.alloc(ASTNode::new((start, end), NodeKind::LitBool(value)))
        }

        rule ident() -> &'a ASTNode<'a> =
            start:position!()
            n:name()
            end:position!()
        {
            arena.alloc(ASTNode::new((start, end), NodeKind::Ident(n)))
        }

        rule primary() -> &'a ASTNode<'a> =
        e:(
            lit_float()
            / lit_int()
            / lit_string()
            / lit_bool()
            / qname()
            / "(" _ e:expr() _ ")" { e }
            / expected!("expression")
        ) { e }

        rule binop() -> &'a ASTNode<'a> = precedence!{
            lhs:(@) _ "=" _ rhs:@ {
                let location = lhs.location.union(rhs.location);
                arena.alloc(ASTNode::new(location, NodeKind::Assign { lhs, rhs, }))
            }
            lhs:(@) _ "+=" _ rhs:@ {
                let location = lhs.location.union(rhs.location);
                arena.alloc(ASTNode::new(location, NodeKind::AssignOp {
                    op: BinaryOp::Add,
                    lhs,
                    rhs,
                }))
            }
            lhs:(@) _ "-=" _ rhs:@ {
                let location = lhs.location.union(rhs.location);
                arena.alloc(ASTNode::new(location, NodeKind::AssignOp {
                    op: BinaryOp::Sub,
                    lhs,
                    rhs,
                }))
            }
            lhs:(@) _ "*=" _ rhs:@ {
                let location = lhs.location.union(rhs.location);
                arena.alloc(ASTNode::new(location, NodeKind::AssignOp {
                    op: BinaryOp::Mul,
                    lhs,
                    rhs,
                }))
            }
            lhs:(@) _ "/=" _ rhs:@ {
                let location = lhs.location.union(rhs.location);
                arena.alloc(ASTNode::new(location, NodeKind::AssignOp {
                    op: BinaryOp::Div,
                    lhs,
                    rhs,
                }))
            }
            lhs:(@) _ "%=" _ rhs:@ {
                let location = lhs.location.union(rhs.location);
                arena.alloc(ASTNode::new(location, NodeKind::AssignOp {
                    op: BinaryOp::Mod,
                    lhs,
                    rhs,
                }))
            }
            lhs:(@) _ "|=" _ rhs:@ {
                let location = lhs.location.union(rhs.location);
                arena.alloc(ASTNode::new(location, NodeKind::AssignOp {
                    op: BinaryOp::BitOr,
                    lhs,
                    rhs,
                }))
            }
            lhs:(@) _ "&=" _ rhs:@ {
                let location = lhs.location.union(rhs.location);
                arena.alloc(ASTNode::new(location, NodeKind::AssignOp {
                    op: BinaryOp::BitAnd,
                    lhs,
                    rhs,
                }))
            }
            --
            lhs:(@) _ "||" _ rhs:@ {
                let location = lhs.location.union(rhs.location);
                arena.alloc(ASTNode::new(location, NodeKind::BinaryExpr {
                    op: BinaryOp::LogOr,
                    lhs,
                    rhs,
                }))
            }
            --
            lhs:(@) _ "&&" _ rhs:@ {
                let location = lhs.location.union(rhs.location);
                arena.alloc(ASTNode::new(location, NodeKind::BinaryExpr {
                    op: BinaryOp::LogAnd,
                    lhs,
                    rhs,
                }))
            }
            --
            lhs:(@) _ "==" _ rhs:@ {
                let location = lhs.location.union(rhs.location);
                arena.alloc(ASTNode::new(location, NodeKind::BinaryExpr {
                    op: BinaryOp::Eq,
                    lhs,
                    rhs,
                }))
            }
            lhs:(@) _ "!=" _ rhs:@ {
                let location = lhs.location.union(rhs.location);
                arena.alloc(ASTNode::new(location, NodeKind::BinaryExpr {
                    op: BinaryOp::Ne,
                    lhs,
                    rhs,
                }))
            }
            lhs:(@) _ ">" _ rhs:@ {
                let location = lhs.location.union(rhs.location);
                arena.alloc(ASTNode::new(location, NodeKind::BinaryExpr {
                    op: BinaryOp::Gt,
                    lhs,
                    rhs,
                }))
            }
            lhs:(@) _ ">=" _ rhs:@ {
                let location = lhs.location.union(rhs.location);
                arena.alloc(ASTNode::new(location, NodeKind::BinaryExpr {
                    op: BinaryOp::Ge,
                    lhs,
                    rhs,
                }))
            }
            lhs:(@) _ "<" _ rhs:@ {
                let location = lhs.location.union(rhs.location);
                arena.alloc(ASTNode::new(location, NodeKind::BinaryExpr {
                    op: BinaryOp::Lt,
                    lhs,
                    rhs,
                }))
            }
            lhs:(@) _ "<=" _ rhs:@ {
                let location = lhs.location.union(rhs.location);
                arena.alloc(ASTNode::new(location, NodeKind::BinaryExpr {
                    op: BinaryOp::Le,
                    lhs,
                    rhs,
                }))
            }
            --
            lhs:(@) _ "|" _ rhs:@ {
                let location = lhs.location.union(rhs.location);
                arena.alloc(ASTNode::new(location, NodeKind::BinaryExpr {
                    op: BinaryOp::BitOr,
                    lhs,
                    rhs,
                }))
            }
            --
            lhs:(@) _ "^" _ rhs:@ {
                let location = lhs.location.union(rhs.location);
                arena.alloc(ASTNode::new(location, NodeKind::BinaryExpr {
                    op: BinaryOp::BitXor,
                    lhs,
                    rhs,
                }))
            }
            --
            lhs:(@) _ "&" _ rhs:@ {
                let location = lhs.location.union(rhs.location);
                arena.alloc(ASTNode::new(location, NodeKind::BinaryExpr {
                    op: BinaryOp::BitAnd,
                    lhs,
                    rhs,
                }))
            }
            --
            lhs:(@) _ "<<" _ rhs:@ {
                let location = lhs.location.union(rhs.location);
                arena.alloc(ASTNode::new(location, NodeKind::BinaryExpr {
                    op: BinaryOp::Shl,
                    lhs,
                    rhs,
                }))
            }
            lhs:(@) _ ">>" _ rhs:@ {
                let location = lhs.location.union(rhs.location);
                arena.alloc(ASTNode::new(location, NodeKind::BinaryExpr {
                    op: BinaryOp::Shr,
                    lhs,
                    rhs,
                }))
            }
            --
            lhs:(@) _ "+" _ rhs:@ {
                let location = lhs.location.union(rhs.location);
                arena.alloc(ASTNode::new(location, NodeKind::BinaryExpr {
                    op: BinaryOp::Add,
                    lhs,
                    rhs,
                }))
            }
            lhs:(@) _ "-" _ rhs:@ {
                let location = lhs.location.union(rhs.location);
                arena.alloc(ASTNode::new(location, NodeKind::BinaryExpr {
                    op: BinaryOp::Sub,
                    lhs,
                    rhs,
                }))
            }
            --
            lhs:(@) _ "*" _ rhs:@ {
                let location = lhs.location.union(rhs.location);
                arena.alloc(ASTNode::new(location, NodeKind::BinaryExpr {
                    op: BinaryOp::Mul,
                    lhs,
                    rhs,
                }))
            }
            lhs:(@) _ "/" _ rhs:@ {
                let location = lhs.location.union(rhs.location);
                arena.alloc(ASTNode::new(location, NodeKind::BinaryExpr {
                    op: BinaryOp::Div,
                    lhs,
                    rhs,
                }))
            }
            lhs:(@) _ "%" _ rhs:@ {
                let location = lhs.location.union(rhs.location);
                arena.alloc(ASTNode::new(location, NodeKind::BinaryExpr {
                    op: BinaryOp::Mod,
                    lhs,
                    rhs,
                }))
            }
            --
            arg:(@) _ "as" _ typ:@ {
                let location = arg.location.union(typ.location);
                arena.alloc(ASTNode::new(location, NodeKind::Cast {
                    arg,
                    typ,
                }))
            }
            --
            "-" arg:(@) {
                arena.alloc(ASTNode::new(arg.location, NodeKind::UnaryExpr {
                    op: UnaryOp::Neg,
                    arg,
                }))
            }
            "!" arg:(@) {
                arena.alloc(ASTNode::new(arg.location, NodeKind::UnaryExpr {
                    op: UnaryOp::Not,
                    arg,
                }))
            }
            "~" arg:(@) {
                arena.alloc(ASTNode::new(arg.location, NodeKind::UnaryExpr {
                    op: UnaryOp::BitNot,
                    arg,
                }))
            }
            func:(@) _ start:position!() "(" _ args:(a: expr() ** (_ "," _) { a }) _ ")" end:position!() {
                let location = func.location.union((start, end));
                arena.alloc(
                    ASTNode::new(location, NodeKind::Call(func, arena.alloc_slice_copy(args.as_slice()))))
            }
            base:(@) _ "." _ start:position!() field:name() end:position!() {
                let location = base.location.union((start, end));
                arena.alloc(
                    ASTNode::new(location, NodeKind::FieldName(base, field)))
            }
            base:(@) _ "." _ start:position!() index:index() end:position!() {
                let location = base.location.union((start, end));
                arena.alloc(
                    ASTNode::new(location, NodeKind::FieldIndex(base, index as usize)))
            }
            p:primary() { p }
        }

        pub rule expr() -> &'a ASTNode<'a> = l:binop() { l } / expected!("expression")

        rule empty_stmt() -> &'a ASTNode<'a> = ";" {
            arena.alloc(ASTNode::new((0, 0), NodeKind::Empty))
        }

        rule qname() -> &'a ASTNode<'a> =
            start:position!()
            n:(ident() ++ (_ "::" _))
            end:position!()
            {
                if n.len() == 1 {
                    n[0]
                } else {
                    let names = arena.alloc_slice_copy(n.as_slice());
                    arena.alloc(ASTNode::new((start, end), NodeKind::QName(names)))
                }
            }

        rule type_name() -> &'a ASTNode<'a> = n:qname() { n }

        rule array_type() -> &'a ASTNode<'a> =
            start:position!()
            "[" _ t:type_expr() _ "]"
            end:position!()
        {
            arena.alloc(ASTNode::new((start, end), NodeKind::ArrayType(t)))
        }

        pub rule type_expr() -> &'a ASTNode<'a> = type_name() / array_type()

        pub rule var_decl() -> &'a ASTNode<'a> =
            start:position!()
            is_const:("let" { false } / "const" { true })
            _ id:name()
            ty:(_ ":" _ ty:type_expr() { ty })?
            init: (_ "=" _ e:expr() { e })?
            _ ";" _
            end:position!() {
                let location = (start, end);
                arena.alloc(ASTNode::new(location, NodeKind::Decl(arena.alloc(ASTDecl::Let {
                    name: id,
                    visibility: decl::DeclVisibility::Private,
                    typ: ty,
                    value: init,
                    is_const
                }))))
            }

        pub rule condition() -> &'a ASTNode<'a> = e:expr() { e } / expected!("condition")

        pub(crate) rule if_stmt() -> &'a ASTNode<'a> =
            start:position!()
            "if"
            _ condition:condition()
            _ then_block:block()
            else_block: (
                _ "else"
                _ s:(
                    s:block() { s }
                    / s:if_stmt() { s }
                ) { s }
            )?
            end:position!()
            {
                let location = (start, end);
                arena.alloc(ASTNode::new(location, NodeKind::If {
                    condition, then_block, else_block
                }))
            }

        rule stmt() -> &'a ASTNode<'a> =
            start:position!()
            _
            s:(
                s0:empty_stmt() { s0 }
                / v:var_decl() { v }
                / i:if_stmt() !(_ "}") [_] { i }
                / b:block() !(_ "}") [_] { b }
                / s:expr() (";" / expected!("semicolon")) { s }
            )
            end:position!()
            { s } / expected!("statement")

        rule block_contents() -> (Vec<&'a ASTNode<'a>>, Option<&'a ASTNode<'a>>)  =
            l:(stmt()*)
            f:(
                _ e: if_stmt() { e }
                / _ e: block() { e }
                / _ e: expr() { e }
            )?
            _
        {
            (l, f)
        }

        pub(crate) rule block() -> &'a ASTNode<'a> =
            start:position!()
            "{"
            c:block_contents()
            "}"
            end:position!()
        {
            let location = (start, end);
            let stmts = arena.alloc_slice_copy(c.0.as_slice());
            arena.alloc(ASTNode::new(location, NodeKind::Block(stmts, c.1)))
        }

        rule visiblity() -> decl::DeclVisibility =
            "pub" { decl::DeclVisibility::Public } / { decl::DeclVisibility::Private }

        rule param_decl() -> &'a ASTFunctionParam<'a> =
            start:position!()
            id:name() _ ":" _ ty:type_expr()
            end:position!()
        {
            let location = (start, end);
            arena.alloc(ASTFunctionParam {
                location: (start, end).into(),
                name: id,
                typ: ty,
            })
        }

        pub rule param_list() -> &'a[&'a ASTFunctionParam<'a>] =
            "(" _
            params:(
                p0:param_decl() _
                p1:("," _ p: param_decl() _ { p })*
                ("," _)?
                {
                    let mut params = Vec::with_capacity(1 + p1.len());
                    params.push(p0);
                    params.extend(p1);
                    arena.alloc_slice_copy(params.as_slice())
                }
            )?
            ")"
            { params.unwrap_or(arena.alloc_slice_copy(&[])) }

        rule func_return() -> &'a ASTNode<'a> = "->" _ t:type_expr() { t }
        rule func_body() -> &'a ASTNode<'a> = t:block() { t }
        rule func_defn() -> &'a ASTNode<'a> =
            start:position!()
            vis:visiblity() _
            native:(("native" _ { true }) / { false })
            "fn" _ id:name() _ p:param_list() _ ret:func_return()? _
            body:(
                b:func_body() { Some(b) }
                / ";" { None }
            )
            end:position!()
        {
            let location = (start, end);
            arena.alloc(ASTNode::new(location, NodeKind::Decl(arena.alloc(ASTDecl::Function {
                name: id,
                visibility: vis,
                params: p,
                ret,
                body,
            }))))
        }

        rule struct_field() -> &'a ASTStructField<'a> =
            start:position!()
            id:name() _ ":" _ ty:type_expr()
            end:position!()
        {
            let location = (start, end);
            arena.alloc(ASTStructField {
                location: (start, end).into(),
                name: id,
                typ: ty,
            })
        }

        pub rule struct_field_list() -> &'a[&'a ASTStructField<'a>] =
            "{" _
            fields:(
                p0:struct_field() _
                p1:("," _ p: struct_field() _ { p })*
                ("," _)?
                {
                    let mut fields = Vec::with_capacity(1 + p1.len());
                    fields.push(p0);
                    fields.extend(p1);
                    arena.alloc_slice_copy(fields.as_slice())
                }
            )?
            "}"
            { fields.unwrap_or(arena.alloc_slice_copy(&[])) }

        rule struct_defn() -> &'a ASTNode<'a> =
            visibility:visiblity() _
            "struct" _ id:name() _
            fields:struct_field_list()
        {
            let decl = arena.alloc(ASTDecl::Struct { name: id, visibility, fields });
            arena.alloc(ASTNode::new((0, 0), NodeKind::Decl(decl)))
        }

        pub rule import_name_list() -> &'a[SmolStr] =
            "{" _
            names:(
                n0:name() _
                n1:("," _ p: name() _ { p })*
                ("," _)?
                {
                    let mut names = Vec::with_capacity(1 + n1.len());
                    names.push(n0);
                    names.extend(n1);
                    arena.alloc_slice_clone(names.as_slice())
                }
            )?
            "}"
            { names.unwrap_or(arena.alloc_slice_clone(&[])) }

        rule import_decl() -> &'a ASTNode<'a> =
            "import" _ id:import_name_list() _ "from" _ path:lit_string() _ ";" _
        {
            todo!();
            // arena.alloc(ASTNode::new((0, 0), NodeKind::Import(path, id)))
        }

        pub rule decl() -> &'a ASTNode<'a>
            = i:import_decl() { i }
            / f:func_defn() { f }
            / s:struct_defn() { s }
            / expected!("declaration")

        pub rule formula() -> &'a ASTNode<'a> =
            start:position!()
            c:block_contents()
            end:position!()
        {
            let location = (start, end);
            let stmts = arena.alloc_slice_copy(c.0.as_slice());
            arena.alloc(ASTNode::new(location, NodeKind::Block(stmts, c.1)))
        }

        pub rule module() -> &'a ASTNode<'a> = _ d:(d:decl() _ { d })* {
            let decls = arena.alloc_slice_copy(d.as_slice());
            arena.alloc(ASTNode::new((0, 0), NodeKind::Module(decls)))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use bumpalo::Bump;

    #[test]
    pub fn literal_int() {
        let arena = Bump::new();
        let ast = formula_parser::expr("1", &arena);
        assert!(matches!(
            ast,
            Ok(ASTNode {
                kind: NodeKind::LitInt(1, IntegerSuffix::Unsized),
                ..
            })
        ));
    }

    #[test]
    pub fn literal_int_i32() {
        let arena = Bump::new();
        let ast = formula_parser::expr("1i32", &arena);
        assert!(matches!(
            ast,
            Ok(ASTNode {
                kind: NodeKind::LitInt(1, IntegerSuffix::I32),
                ..
            })
        ));
    }

    #[test]
    pub fn literal_int_i64() {
        let arena = Bump::new();
        let ast = formula_parser::expr("1i64", &arena);
        assert!(matches!(
            ast,
            Ok(ASTNode {
                kind: NodeKind::LitInt(1, IntegerSuffix::I64),
                ..
            })
        ));
    }

    #[test]
    pub fn literal_float() {
        let arena = Bump::new();
        let ast = formula_parser::expr("1.0", &arena);
        assert!(matches!(
            ast,
            Ok(ASTNode {
                kind: NodeKind::LitFloat(1.0, FloatSuffix::F32),
                ..
            })
        ));

        let ast = formula_parser::expr("1.", &arena);
        assert!(matches!(
            ast,
            Ok(ASTNode {
                kind: NodeKind::LitFloat(1., FloatSuffix::F32),
                ..
            })
        ));

        let ast = formula_parser::expr("1.0e10", &arena);
        assert!(matches!(
            ast,
            Ok(ASTNode {
                kind: NodeKind::LitFloat(1.0e10, FloatSuffix::F32),
                ..
            })
        ));

        let ast = formula_parser::expr("1.0f32", &arena);
        assert!(matches!(
            ast,
            Ok(ASTNode {
                kind: NodeKind::LitFloat(1.0, FloatSuffix::F32),
                ..
            })
        ));

        let ast = formula_parser::expr("1.0f64", &arena);
        assert!(matches!(
            ast,
            Ok(ASTNode {
                kind: NodeKind::LitFloat(1.0, FloatSuffix::F64),
                ..
            })
        ));
    }

    #[test]
    pub fn literal_string() {
        let arena = Bump::new();
        let ast = formula_parser::expr("\"hello\"", &arena).unwrap();
        match ast {
            ASTNode {
                kind: NodeKind::LitString(s),
                ..
            } => {
                assert_eq!(s, "hello");
            }
            _ => panic!(),
        };

        let ast = formula_parser::expr("\"hello\\n\"", &arena).unwrap();
        match ast {
            ASTNode {
                kind: NodeKind::LitString(s),
                ..
            } => {
                assert_eq!(s, "hello\n");
            }
            _ => panic!(),
        };

        let ast = formula_parser::expr("\"hello\\\"\"", &arena).unwrap();
        match ast {
            ASTNode {
                kind: NodeKind::LitString(s),
                ..
            } => {
                assert_eq!(s, "hello\"");
            }
            _ => panic!(),
        };

        let ast = formula_parser::expr("\"hello\u{25ff}\"", &arena).unwrap();
        match ast {
            ASTNode {
                kind: NodeKind::LitString(s),
                ..
            } => {
                assert_eq!(s, "hello◿");
            }
            _ => panic!(),
        };
    }

    #[test]
    pub fn binop_add() {
        let arena = Bump::new();
        let ast = formula_parser::expr("1.0 + 5", &arena);
        match ast {
            Ok(ASTNode {
                kind: NodeKind::BinaryExpr { op, lhs, rhs },
                location,
                ..
            }) => {
                assert_eq!(location.start(), 0);
                assert_eq!(location.end(), 7);
                assert_eq!(*op, BinaryOp::Add);
                assert!(matches!(
                    lhs.kind,
                    NodeKind::LitFloat(1.0, FloatSuffix::F32)
                ));
                assert!(matches!(
                    rhs.kind,
                    NodeKind::LitInt(5, IntegerSuffix::Unsized)
                ));
            }
            _ => panic!(),
        }
    }

    #[test]
    pub fn binop_err() {
        let arena = Bump::new();
        let ast = formula_parser::expr("1.0 + +", &arena).unwrap_err();
        // println!("{:?}", ast);
        assert_eq!(ast.location.offset, 6);
        // println!("{:?}", ast.expected);
    }

    #[test]
    pub fn param_list() {
        let arena = Bump::new();
        let ast = formula_parser::param_list("()", &arena).unwrap();
        assert_eq!(ast.len(), 0);

        let ast = formula_parser::param_list("(a: bool)", &arena).unwrap();
        assert_eq!(ast.len(), 1);

        let ast = formula_parser::param_list("(a: bool,)", &arena).unwrap();
        assert_eq!(ast.len(), 1);

        let ast = formula_parser::param_list("(a: bool, b: bool)", &arena).unwrap();
        assert_eq!(ast.len(), 2);

        let ast = formula_parser::param_list("(a: bool, b: bool,)", &arena).unwrap();
        assert_eq!(ast.len(), 2);
    }

    #[test]
    pub fn block() {
        let arena = Bump::new();

        let ast = formula_parser::block("{}", &arena).unwrap();
        let NodeKind::Block(stmts, result) = ast.kind else {
            panic!()
        };
        assert_eq!(stmts.len(), 0);
        assert!(result.is_none());

        let ast = formula_parser::block("{ 10 }", &arena).unwrap();
        let NodeKind::Block(stmts, result) = ast.kind else {
            panic!()
        };
        assert_eq!(stmts.len(), 0);
        assert!(result.is_some());

        let ast = formula_parser::block("{ 10; }", &arena).unwrap();
        let NodeKind::Block(stmts, result) = ast.kind else {
            panic!()
        };
        assert_eq!(stmts.len(), 1);
        assert!(result.is_none());

        let ast = formula_parser::block("{ 10; 10 }", &arena).unwrap();
        let NodeKind::Block(stmts, result) = ast.kind else {
            panic!()
        };
        assert_eq!(stmts.len(), 1);
        assert!(result.is_some());

        let ast = formula_parser::block("{ if true { 0 } }", &arena).unwrap();
        let NodeKind::Block(stmts, result) = ast.kind else {
            panic!()
        };
        assert_eq!(stmts.len(), 0);
        assert!(result.is_some());

        let ast = formula_parser::block("{ if true { 0 } 10 }", &arena).unwrap();
        let NodeKind::Block(stmts, result) = ast.kind else {
            panic!()
        };
        assert_eq!(stmts.len(), 1);
        assert!(result.is_some());

        let ast = formula_parser::block("{ { 0 } }", &arena).unwrap();
        let NodeKind::Block(stmts, result) = ast.kind else {
            panic!()
        };
        assert_eq!(stmts.len(), 0);
        assert!(result.is_some());

        let ast = formula_parser::block("{ { 1 } { 0 } }", &arena).unwrap();
        let NodeKind::Block(stmts, result) = ast.kind else {
            panic!()
        };
        assert_eq!(stmts.len(), 1);
        assert!(result.is_some());

        let ast = formula_parser::formula("{ 0 }", &arena).unwrap();
        let NodeKind::Block(stmts, result) = ast.kind else {
            panic!()
        };
        assert_eq!(stmts.len(), 0);
        assert!(result.is_some());
    }

    #[test]
    pub fn if_stmt() {
        let arena = Bump::new();
        let ast = formula_parser::if_stmt("if x > 0 { 1.0 }", &arena);
        match ast {
            Ok(ASTNode {
                kind:
                    NodeKind::If {
                        condition: test,
                        then_block,
                        else_block,
                    },
                location,
                ..
            }) => {
                assert_eq!(location.start(), 0);
                assert_eq!(location.end(), 16);
                assert!(matches!(test.kind, NodeKind::BinaryExpr { .. }));
                assert!(matches!(then_block.kind, NodeKind::Block(_, _)));
                assert!(else_block.is_none());
            }
            Err(err) => panic!("Parse error: {err} at {}", err.location),
            _ => panic!("Unexpected AST: {ast:?}"),
        }
    }

    #[test]
    pub fn if_else_stmt() {
        let arena = Bump::new();
        let ast = formula_parser::if_stmt("if x > 0 { 1.0 } else { 2.0 }", &arena);
        match ast {
            Ok(ASTNode {
                kind:
                    NodeKind::If {
                        condition: test,
                        then_block,
                        else_block,
                    },
                location,
                ..
            }) => {
                assert_eq!(location.start(), 0);
                assert_eq!(location.end(), 29);
                assert!(matches!(test.kind, NodeKind::BinaryExpr { .. }));
                assert!(matches!(then_block.kind, NodeKind::Block(_, _)));
                assert!(matches!(else_block.unwrap().kind, NodeKind::Block(_, _)));
            }
            Err(err) => panic!("Parse error: {err} at {}", err.location),
            _ => panic!("Unexpected AST: {ast:?}"),
        }
    }
}
