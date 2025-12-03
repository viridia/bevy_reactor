use crate::{
    ast::{self, ASTNode, NodeKind},
    compiler::CompilationError,
    decl::{self, Scope},
    expr_type::ExprType,
};

/// Convert AST types to type expressions.
pub(crate) fn resolve_types<'a>(
    scope: &Scope,
    ast: &'a ASTNode<'a>,
) -> Result<ExprType, CompilationError> {
    match &ast.kind {
        NodeKind::ArrayType(_member) => todo!(),
        NodeKind::Ident(ident) => {
            let Some(decl) = scope.lookup(ident.as_str()) else {
                return Err(CompilationError::UnknownType(
                    ast.location,
                    ident.to_string(),
                ));
            };
            match &decl.kind {
                decl::DeclKind::TypeAlias(typ) => Ok(typ.clone()),
                // decl::Decl::Struct(sindex) => {
                //     Ok(ExprType::Struct(decls.structs[*sindex].typ.clone()))
                // }
                _ => panic!("Invalid type expression: {decl:?}",),
            }
        }
        NodeKind::Empty => Ok(ExprType::None),
        _ => {
            panic!("Invalid type expression: {:?}", ast.kind)
        }
    }
}
