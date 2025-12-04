use core::result;
use std::sync::Arc;

use crate::Module;
use crate::ast::{ASTNode, FloatSuffix, IntegerSuffix, NodeKind};
use crate::decl::{Decl, DeclKind, DeclTable, Scope};
use crate::expr_type::ExprType;
use crate::host::HostState;
use bevy::{log::info, render::render_graph::Node, scene::ron::de};

use crate::{
    ast::ASTDecl,
    compiler::{self, CompilationError, CompiledFunction, FunctionBody, ModuleExprs},
    decl::{self, DeclVisibility, FunctionParam},
    expr::{Expr, ExprKind},
    expr_type::FunctionType,
    oper::{BinaryOp, UnaryOp},
};

use super::{
    assign_types::assign_types, resolve_types::resolve_types, type_inference::TypeInference,
};

pub(crate) fn build_module_exprs<'ast, 'me>(
    host: &HostState,
    module: &mut Module,
    ast: &'ast ASTNode<'ast>,
    expr_arena: &'me bumpalo::Bump,
) -> Result<ModuleExprs<'me>, CompilationError> {
    if let NodeKind::Module(ast_decls) = &ast.kind {
        let mut module_exprs = ModuleExprs::default();

        // Construct all the top-level declarations first
        create_decls(module, &mut module_exprs, ast_decls)?;
        resolve_decl_types(host, module, ast_decls)?;
        build_function_bodies(host, module, ast_decls, &mut module_exprs, expr_arena)?;

        Ok(module_exprs)
    } else {
        panic!("Invalid AST node for compilation unit: {:?}", ast.kind);
    }
}

// pub(crate) fn _build_module_exprs<'ast>(ast: &'ast ASTNode<'ast>) -> Result<(), CompilationError> {
//     // Resolve types for all declarations.
//     if let NodeKind::Module(ast_decls) = &ast.kind {
//         // Build function body expressions.
//         for decl_ast in *ast_decls {
//             if let NodeKind::Decl(ASTDecl::Function {
//                 name,
//                 body: body_ast,
//                 ..
//             }) = &decl_ast.kind
//             {
//                 let mut local_scope = Scope::new(Some(&param_scope));
//                 let mut locals_table: Vec<LocalDecl> = Vec::new();
//                 let mut body_expr = if let Some(body_ast) = body_ast {
//                     if function.is_native {
//                         return Err(CompilationError::NativeFunctionHasBody(function.location));
//                     }
//                     build_exprs(
//                         body_ast,
//                         &mut local_scope,
//                         decls,
//                         &mut locals_table,
//                         &mut inference,
//                     )?
//                 } else {
//                     if !function.is_native {
//                         return Err(CompilationError::MissingBody(function.location));
//                     }
//                     Expr::new(decl_ast.location, ExprKind::Empty)
//                 };
//                 let function = &mut decls.functions[*findex];
//                 if body_ast.is_some() {
//                     inference.add_constraint(
//                         function.typ.ret.clone(),
//                         body_expr.typ.clone(),
//                         body_expr.location,
//                     );
//                     inference.solve_constraints()?;
//                     assign_types(&mut body_expr, &inference)?;
//                     for local in locals_table.iter_mut() {
//                         local.typ = inference.substitute(&local.typ);
//                     }
//                 }
//                 function.body = body_expr;
//                 function.locals = locals_table;
//             }
//         }

//         Ok(())
//     } else {
//         panic!("Invalid AST node for compilation unit: {:?}", ast.kind);
//     }
// }

pub(crate) fn build_formula_exprs<'ast, 'me>(
    host: &HostState,
    module: &mut Module,
    ast: &'ast ASTNode<'ast>,
    ret_type: ExprType,
    expr_arena: &'me bumpalo::Bump,
) -> Result<ModuleExprs<'me>, CompilationError> {
    // The 'formula' parsing rule produces a `Block` AST.
    if let NodeKind::Block(stmts, result) = &ast.kind {
        let mut module_exprs = ModuleExprs::default();

        let host_scope = Scope {
            parent: None,
            decls: &host.global_decls,
        };

        // Multiple declarations of the same function are not allowed.
        if module.module_decls.contains_key(Module::DEFAULT) {
            // This should never happen since this is only called once per compilation unit.
            return Err(CompilationError::FunctionRedefinition(
                ast.location,
                Module::DEFAULT.to_string(),
            ));
        }

        // Hoisting: function and type declarations are processed first
        let mut hoisted: Vec<&ASTNode> = Vec::new();
        let mut non_hoisted: Vec<&ASTNode> = Vec::new();

        for s in stmts.iter() {
            if let NodeKind::Decl(decl) = s.kind
                && let ASTDecl::Function { .. } = decl
            {
                hoisted.push(s);
            } else {
                non_hoisted.push(s);
            }
        }

        // Construct all the hoisted declarations first
        create_decls(module, &mut module_exprs, &hoisted)?;
        resolve_decl_types(host, module, &hoisted)?;
        build_function_bodies(host, module, &hoisted, &mut module_exprs, expr_arena)?;

        let function_index = module_exprs.functions.len();
        let fd = decl::Decl {
            location: ast.location,
            visibility: DeclVisibility::Private,
            kind: DeclKind::Function {
                params: Vec::new(),
                ret: ret_type.clone(),
                is_native: false,
                index: function_index,
            },
        };
        module_exprs.functions.push(FunctionBody { body: None });

        let mut inference: TypeInference = Default::default();
        let mut local_index: usize = 0;
        // let mut locals_table: Vec<LocalDecl> = Vec::new();

        // Scope for the whole module.
        let module_scope = Scope {
            parent: Some(&host_scope),
            decls: &module.module_decls,
        };

        // Scope for the function parameters from within the function body.
        let param_decls = DeclTable::default();
        let param_scope = Scope {
            parent: Some(&module_scope),
            decls: &param_decls,
        };

        let body_expr = build_block(
            ast,
            host,
            &param_scope,
            &mut local_index,
            &mut inference,
            expr_arena,
            &non_hoisted,
            result,
        )?;

        inference.add_constraint(ret_type.clone(), body_expr.typ.clone(), body_expr.location);
        inference.solve_constraints()?;
        assign_types(body_expr, &inference)?;
        module_exprs.functions[function_index].body = Some(body_expr);

        module.module_decls.insert(Module::DEFAULT.into(), fd);
        Ok(module_exprs)
    } else {
        panic!("Invalid AST node for formula: {:?}", ast.kind);
    }
}

/// Populate the module scope with all of the declarations.
fn create_decls<'ast, 'me>(
    module: &mut Module,
    module_exprs: &mut ModuleExprs<'me>,
    ast_decls: &'ast [&'ast ASTNode<'ast>],
) -> Result<(), CompilationError> {
    for ast_decl in ast_decls {
        match &ast_decl.kind {
            // NodeKind::Import(_, _) => {}
            NodeKind::Decl(d) => match d {
                ASTDecl::Function {
                    name, visibility, ..
                } => {
                    // Multiple declarations of the same function are not allowed.
                    if module.module_decls.contains_key(name) {
                        return Err(CompilationError::FunctionRedefinition(
                            ast_decl.location,
                            name.to_string(),
                        ));
                    }

                    // Note: fill in param and return type later, once the module scope
                    // is fully populated with type definitions.
                    let function_index = module_exprs.functions.len();
                    let fd = decl::Decl {
                        location: ast_decl.location,
                        visibility: *visibility,
                        kind: DeclKind::Function {
                            params: Vec::new(),
                            ret: ExprType::default(),
                            is_native: false,
                            index: function_index,
                        },
                    };
                    module_exprs.functions.push(FunctionBody { body: None });
                    module.module_decls.insert(name.clone(), fd);
                }

                ASTDecl::Let {
                    name,
                    visibility: _,
                    is_const: _,
                    ..
                } => {
                    // Multiple declarations of the same function are not allowed.
                    if module.module_decls.contains_key(name) {
                        return Err(CompilationError::NameRedefinition(
                            ast_decl.location,
                            name.to_string(),
                        ));
                    }

                    todo!();
                    // let index = decls.globals.len();
                    // let gd = decl::GlobalDecl {
                    //     location: ast_decl.location,
                    //     visibility: *visibility,
                    //     name: *name,
                    //     typ: ExprType::None,
                    //     is_const: *is_const,
                    //     index,
                    // };

                    // decls.globals.push(gd);
                    // scope.insert(*name, decl::Decl::Global(index));
                } // ASTDecl::Struct {
                  //     name, visibility, ..
                  // } => {
                  //     // Multiple declarations of the same function are not allowed.
                  //     if scope.contains(*name) {
                  //         let name_str = decls.symbols.resolve(*name);
                  //         return Err(CompilationError::NameRedefinition(
                  //             ast_decl.location,
                  //             name_str,
                  //         ));
                  //     }

                  //     let index = decls.structs.len();
                  //     let sd = decl::StructDecl {
                  //         location: ast_decl.location,
                  //         name: *name,
                  //         visibility: *visibility,
                  //         typ: Arc::new(StructType::default()),
                  //         index,
                  //     };

                  //     decls.structs.push(sd);
                  //     scope.insert(*name, decl::Decl::Struct(index));
                  // } // ASTDecl::TypeAlias { .. } => todo!(),
            },
            _ => panic!("Invalid AST node for declaration: {:?}", ast_decl.kind),
        }
    }

    Ok(())
}

/// Assign type information to the parameter and return types of each declaration.
pub(crate) fn resolve_decl_types<'ast>(
    host: &HostState,
    module: &mut Module,
    ast_decls: &'ast [&'ast ASTNode<'ast>],
) -> Result<(), CompilationError> {
    let host_scope = Scope {
        parent: None,
        decls: &host.global_decls,
    };

    for ast_decl in ast_decls {
        match &ast_decl.kind {
            // NodeKind::Import(_, _) => {}
            NodeKind::Decl(d) => match d {
                ASTDecl::Function {
                    name,
                    params,
                    ret: ret_ast,
                    ..
                } => {
                    let module_scope = Scope {
                        parent: Some(&host_scope),
                        decls: &module.module_decls,
                    };

                    let ret_type = match ret_ast {
                        None => ExprType::Void,
                        Some(ret_ast) => resolve_types(&module_scope, ret_ast)?,
                    };

                    let mut params_mapped: Vec<FunctionParam> = Vec::with_capacity(params.len());
                    for (i, p) in params.iter().enumerate() {
                        let typ = resolve_types(&module_scope, p.typ)?;
                        params_mapped.push(FunctionParam {
                            location: p.location,
                            name: p.name.clone(),
                            typ,
                            index: i,
                            local_index: 0,
                        });
                    }

                    let decl = module.module_decls.get_mut(name).unwrap();
                    let DeclKind::Function {
                        params: fd_params,
                        ret: fd_ret,
                        ..
                    } = &mut decl.kind
                    else {
                        panic!("Expected function declaration");
                    };

                    *fd_params = params_mapped;
                    *fd_ret = ret_type;
                }
                ASTDecl::Let { .. } => todo!(),
                // ASTDecl::Struct {
                //     name,
                //     is_record,
                //     fields,
                //     ..
                // } => {
                //     let decl = scope.get(*name).unwrap();
                //     let decl::Decl::Struct(sindex) = decl else {
                //         unreachable!()
                //     };

                //     let mut stype = StructType {
                //         name: *name,
                //         is_record: *is_record,
                //         fields: Vec::with_capacity(fields.len()),
                //     };

                //     for (i, f) in fields.iter().enumerate() {
                //         let typ = resolve_types(decls, scope, f.typ)?;
                //         stype.fields.push(decl::FieldDecl {
                //             location: f.location,
                //             name: f.name,
                //             typ,
                //             index: i,
                //         });
                //     }

                //     let sd = &mut decls.structs[*sindex];
                //     sd.typ = Arc::new(stype);
                // } // ASTDecl::TypeAlias { .. } => todo!(),
            },
            _ => panic!("Invalid AST node for declaration: {:?}", ast_decl.kind),
        }
    }
    Ok(())
}

/// Assign type information to the parameter and return types of each declaration.
pub(crate) fn build_function_bodies<'ast, 'me>(
    host: &HostState,
    module: &mut Module,
    ast_decls: &'ast [&'ast ASTNode<'ast>],
    module_exprs: &mut ModuleExprs<'me>,
    expr_arena: &'me bumpalo::Bump,
) -> Result<(), CompilationError> {
    let host_scope = Scope {
        parent: None,
        decls: &host.global_decls,
    };

    for ast_decl in ast_decls {
        if let NodeKind::Decl(d) = ast_decl.kind
            && let ASTDecl::Function { name, body, .. } = d
        {
            let decl = module.module_decls.get_mut(name).unwrap();
            let DeclKind::Function {
                params: fd_params,
                ret: fd_ret,
                index: fd_index,
                ..
            } = &mut decl.kind
            else {
                panic!("Expected function declaration");
            };
            let fd_ret = fd_ret.clone();
            let fd_index = *fd_index;

            let Some(body_ast) = body else {
                continue;
            };

            // Build parameter scope
            let mut param_decls = DeclTable::default();
            for param in fd_params {
                param_decls.insert(
                    param.name.clone(),
                    Decl {
                        location: param.location,
                        visibility: DeclVisibility::Public,
                        kind: DeclKind::Param {
                            typ: param.typ.clone(),
                            index: param.index,
                        },
                    },
                );
            }

            let module_scope = Scope {
                parent: Some(&host_scope),
                decls: &module.module_decls,
            };

            let param_scope = Scope {
                parent: Some(&module_scope),
                decls: &param_decls,
            };

            let mut local_decls = DeclTable::default();

            let mut inference: TypeInference = Default::default();
            let mut local_index: usize = 0;
            let body_expr = build_exprs(
                body_ast,
                host,
                &param_scope,
                &mut local_decls,
                &mut local_index,
                &mut inference,
                expr_arena,
            )?;

            // fn build_exprs<'a, 'e>(
            //     ast: &'a ASTNode<'a>,
            //     host: &HostState,
            //     parent_scope: &decl::Scope,
            //     current_scope: &mut decl::DeclTable,
            //     locals: &mut usize,
            //     inference: &mut TypeInference,
            //     out: &'e bumpalo::Bump,
            // ) -> Result<&'e mut Expr<'e>, CompilationError> {

            inference.add_constraint(fd_ret, body_expr.typ.clone(), body_expr.location);
            inference.solve_constraints()?;
            assign_types(body_expr, &inference)?;
            module_exprs.functions[fd_index].body = Some(body_expr);
        }
    }

    Ok(())
}

fn build_exprs<'a, 'e>(
    ast: &'a ASTNode<'a>,
    host: &HostState,
    parent_scope: &decl::Scope,
    current_scope: &mut decl::DeclTable,
    locals: &mut usize,
    inference: &mut TypeInference,
    out: &'e bumpalo::Bump,
) -> Result<&'e mut Expr<'e>, CompilationError> {
    match &ast.kind {
        NodeKind::LitInt(value, suffix) => {
            let typ = match suffix {
                &crate::ast::IntegerSuffix::Unsized => {
                    let ty = inference.fresh_typevar();
                    if *value > i32::MAX as i64 || *value < i32::MIN as i64 {
                        inference.add_constraint(ty.clone(), ExprType::I64, ast.location);
                    } else {
                        inference.add_constraint(ty.clone(), ExprType::I32, ast.location);
                    }
                    ty
                }
                crate::ast::IntegerSuffix::I32 => ExprType::I32,
                crate::ast::IntegerSuffix::I64 => ExprType::I64,
            };
            Ok(out
                .alloc(Expr::new(ast.location, ExprKind::ConstInteger(*value)))
                .with_type(typ))
        }

        NodeKind::LitFloat(value, suffix) => {
            // let value = value.parse::<f64>().unwrap();
            let typ = match suffix {
                crate::ast::FloatSuffix::F32 => ExprType::F32,
                crate::ast::FloatSuffix::F64 => ExprType::F64,
            };
            Ok(out
                .alloc(Expr::new(ast.location, ExprKind::ConstFloat(*value)))
                .with_type(typ))
        }

        NodeKind::LitString(value) => Ok(out
            .alloc(Expr::new(
                ast.location,
                ExprKind::ConstString(value.clone()),
            ))
            .with_type(ExprType::String)),

        NodeKind::LitBool(value) => Ok(out
            .alloc(Expr::new(ast.location, ExprKind::ConstBool(*value)))
            .with_type(ExprType::Boolean)),

        NodeKind::Ident(name) => match parent_scope.lookup(name) {
            Some(decl) => {
                match &decl.kind {
                    decl::DeclKind::Function {
                        params: _,
                        ret: _,
                        is_native: _,
                        index: _,
                    } => {
                        todo!();
                        // let function = &decls.functions[*findex];
                        // // println!("Calling function: {:?}", function.index);
                        // Ok(
                        //     Expr::new(ast.location, ExprKind::FunctionRef(function.function_index))
                        //         .with_type(ExprType::Function(function.typ.clone())),
                        // )
                    }
                    decl::DeclKind::Local {
                        typ,
                        is_const: _,
                        index,
                    } => Ok(out
                        .alloc(Expr::new(ast.location, ExprKind::LocalRef(*index)))
                        .with_type(typ.clone())),
                    decl::DeclKind::Global {
                        typ,
                        is_const: _,
                        index,
                    } => Ok(out
                        .alloc(Expr::new(ast.location, ExprKind::GlobalRef(*index)))
                        .with_type(typ.clone())),
                    decl::DeclKind::Param { typ, index } => Ok(out
                        .alloc(Expr::new(ast.location, ExprKind::ParamRef(*index)))
                        .with_type(typ.clone())),
                    // Struct (constructor)
                    // Enum (constructor)
                    _ => todo!("Ident: {:?}", decl),
                }
            }
            None => Err(CompilationError::UnknownSymbol(
                ast.location,
                name.to_string(),
            )),
        },

        NodeKind::QName(_name) => panic!("Should already be resolved"),

        NodeKind::BinaryExpr { op, lhs, rhs } => {
            let lhs_expr = build_exprs(
                lhs,
                host,
                parent_scope,
                current_scope,
                locals,
                inference,
                out,
            )?;
            let rhs_expr = build_exprs(
                rhs,
                host,
                parent_scope,
                current_scope,
                locals,
                inference,
                out,
            )?;
            let ty = match op {
                BinaryOp::Add
                | BinaryOp::Sub
                | BinaryOp::Mul
                | BinaryOp::Div
                | BinaryOp::Mod
                | BinaryOp::BitAnd
                | BinaryOp::BitOr
                | BinaryOp::BitXor => {
                    let ty = inference.fresh_typevar();
                    // Both sides must be the same, which is also the result type.
                    inference.add_constraint(ty.clone(), lhs_expr.typ.clone(), lhs_expr.location);
                    inference.add_constraint(ty.clone(), rhs_expr.typ.clone(), rhs_expr.location);
                    ty
                }

                BinaryOp::LogAnd | BinaryOp::LogOr => {
                    // Both sides must be boolean.
                    inference.add_constraint(
                        ExprType::Boolean,
                        lhs_expr.typ.clone(),
                        lhs_expr.location,
                    );
                    inference.add_constraint(
                        ExprType::Boolean,
                        rhs_expr.typ.clone(),
                        rhs_expr.location,
                    );
                    ExprType::Boolean
                }

                BinaryOp::Shl | BinaryOp::Shr => todo!(),

                BinaryOp::Eq | BinaryOp::Ne => {
                    let ty = inference.fresh_typevar();
                    // Both sides must be the same, but the result type is Boolean.
                    inference.add_constraint(ty.clone(), lhs_expr.typ.clone(), lhs_expr.location);
                    inference.add_constraint(ty.clone(), rhs_expr.typ.clone(), rhs_expr.location);
                    ExprType::Boolean
                }

                BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => {
                    let ty = inference.fresh_typevar();
                    // Both sides must be the same, but the result type is Boolean.
                    inference.add_constraint(ty.clone(), lhs_expr.typ.clone(), lhs_expr.location);
                    inference.add_constraint(ty.clone(), rhs_expr.typ.clone(), rhs_expr.location);
                    ExprType::Boolean
                }
            };

            Ok(out
                .alloc(Expr::new(
                    ast.location,
                    ExprKind::BinaryExpr {
                        op: *op,
                        lhs: lhs_expr,
                        rhs: rhs_expr,
                    },
                ))
                .with_type(ty))
        }

        NodeKind::Assign { lhs, rhs } => {
            let lhs_expr = build_exprs(
                lhs,
                host,
                parent_scope,
                current_scope,
                locals,
                inference,
                out,
            )?;
            let rhs_expr = build_exprs(
                rhs,
                host,
                parent_scope,
                current_scope,
                locals,
                inference,
                out,
            )?;

            inference.add_constraint(
                lhs_expr.typ.clone(),
                rhs_expr.typ.clone(),
                lhs_expr.location,
            );
            Ok(out
                .alloc(Expr::new(
                    ast.location,
                    ExprKind::Assign {
                        lhs: lhs_expr,
                        rhs: rhs_expr,
                    },
                ))
                .with_type(ExprType::Void)) // Don't support chained assignments.
        }

        NodeKind::AssignOp { op, lhs, rhs } => {
            let lhs_expr = build_exprs(
                lhs,
                host,
                parent_scope,
                current_scope,
                locals,
                inference,
                out,
            )?;
            let rhs_expr = build_exprs(
                rhs,
                host,
                parent_scope,
                current_scope,
                locals,
                inference,
                out,
            )?;
            match op {
                BinaryOp::Add
                | BinaryOp::Sub
                | BinaryOp::Mul
                | BinaryOp::Div
                | BinaryOp::Mod
                | BinaryOp::BitAnd
                | BinaryOp::BitOr
                | BinaryOp::BitXor => {
                    let ty = inference.fresh_typevar();
                    // Both sides must be the same, which is also the result type.
                    inference.add_constraint(ty.clone(), lhs_expr.typ.clone(), lhs_expr.location);
                    inference.add_constraint(ty.clone(), rhs_expr.typ.clone(), rhs_expr.location);
                }

                // BinaryOp::LogAnd | BinaryOp::LogOr => {
                //     // Both sides must be boolean.
                //     inference.add_constraint(
                //         ExprType::Boolean,
                //         lhs_expr.typ.clone(),
                //         lhs_expr.location,
                //     );
                //     inference.add_constraint(
                //         ExprType::Boolean,
                //         rhs_expr.typ.clone(),
                //         rhs_expr.location,
                //     );
                // }
                BinaryOp::Shl | BinaryOp::Shr => todo!(),

                _ => panic!("Invalid augmented assignment operator: {op:?}"),
            };

            Ok(out
                .alloc(Expr::new(
                    ast.location,
                    ExprKind::AssignOp {
                        op: *op,
                        lhs: lhs_expr,
                        rhs: rhs_expr,
                    },
                ))
                .with_type(ExprType::Void)) // Don't support chained assignments.
        }

        NodeKind::UnaryExpr { op, arg } => {
            let arg_expr = build_exprs(
                arg,
                host,
                parent_scope,
                current_scope,
                locals,
                inference,
                out,
            )?;
            let arg_expr = out.alloc(arg_expr);
            let ty = match op {
                UnaryOp::Not => {
                    inference.add_constraint(
                        ExprType::Boolean,
                        arg_expr.typ.clone(),
                        arg_expr.location,
                    );
                    ExprType::Boolean
                }
                UnaryOp::Neg => arg_expr.typ.clone(),
                UnaryOp::BitNot => arg_expr.typ.clone(),
            };

            Ok(out
                .alloc(Expr::new(
                    ast.location,
                    ExprKind::UnaryExpr {
                        op: *op,
                        arg: arg_expr,
                    },
                ))
                .with_type(ty))
        }

        NodeKind::FieldName(base, fname) => {
            let base_expr = build_exprs(
                base,
                host,
                parent_scope,
                current_scope,
                locals,
                inference,
                out,
            )?;
            #[allow(clippy::match_single_binding)] // For now
            match base_expr.typ.clone() {
                ExprType::Entity => {
                    if let Some(field) = host.entity_decls.get(fname) {
                        match &field.kind {
                            DeclKind::Global {
                                typ,
                                is_const: _,
                                index,
                            } => {
                                // assign_types(arg_expr, &infer)?;
                                Ok(out
                                    .alloc(Expr::new(
                                        ast.location,
                                        ExprKind::EntityProp(base_expr, *index),
                                    ))
                                    .with_type(typ.clone()))
                                // builder.push_op(instr::OP_LOAD_ENTITY_PROP);
                                // builder.push_immediate::<u32>(health_id as u32);
                            }
                            DeclKind::Function {
                                params: _,
                                ret: _,
                                is_native: _,
                                index: _,
                            } => todo!(),
                            _ => panic!("Invalid entity member"),
                        }
                    } else {
                        Err(CompilationError::UnknownField(
                            ast.location,
                            "Entity".to_string(),
                            fname.to_string(),
                        ))
                    }
                }
                // ExprType::Struct(stype) => {
                //     let field = stype.fields.iter().find(|field| field.name == *fname);
                //     if let Some(field) = field {
                //         Ok(Expr::new(
                //             ast.location,
                //             ExprKind::Field(Box::new(base_expr), field.index),
                //         )
                //         .with_type(field.typ.clone()))
                //     } else {
                //         Err(CompilationError::UnknownField(
                //             ast.location,
                //             decls.symbols.resolve(stype.name),
                //             decls.symbols.resolve(*fname),
                //         ))
                //     }
                // }
                _ => Err(CompilationError::NoFields(
                    ast.location,
                    base_expr.typ.clone(),
                )),
            }
        }

        NodeKind::FieldIndex(base, _index) => {
            let base_expr = build_exprs(
                base,
                host,
                parent_scope,
                current_scope,
                locals,
                inference,
                out,
            )?;
            #[allow(clippy::match_single_binding)] // For now
            match base_expr.typ.clone() {
                // ExprType::TupleStruct(tstype) => {
                //     if *index >= tstype.fields.len() {
                //         return Err(CompilationError::InvalidIndex(
                //             ast.location,
                //             decls.symbols.resolve(tstype.name),
                //             *index,
                //         ));
                //     }
                //     let field = tstype.fields[*index].clone();
                //     Ok(
                //         Expr::new(ast.location, ExprKind::Index(Box::new(base_expr), *index))
                //             .with_type(field),
                //     )
                // }
                _ => Err(CompilationError::NoFields(
                    ast.location,
                    base_expr.typ.clone(),
                )),
            }
        }

        NodeKind::Empty => Ok(out.alloc(Expr::new(ast.location, ExprKind::Empty))),
        NodeKind::Decl(decl) => match decl {
            ASTDecl::Let {
                name,
                typ,
                value,
                is_const: _,
                visibility: _,
                ..
            } => {
                let typ = match typ {
                    Some(typ) => Some(resolve_types(parent_scope, typ)?),
                    None => None,
                };
                let value_expr = match value {
                    Some(value) => Some(build_exprs(
                        value,
                        host,
                        parent_scope,
                        current_scope,
                        locals,
                        inference,
                        out,
                    )?),
                    None => None,
                };
                let ty = match (typ, &value_expr) {
                    (Some(typ), Some(value)) => {
                        inference.add_constraint(typ.clone(), value.typ.clone(), value.location);
                        typ
                    }
                    (Some(typ), None) => typ,
                    (None, Some(value)) => value.typ.clone(),
                    (None, None) => {
                        return Err(CompilationError::MissingType(
                            ast.location,
                            name.to_string(),
                        ));
                    }
                };

                todo!();
                // let index = locals.len();
                // let local = decl::LocalDecl {
                //     location: ast.location,
                //     visibility: *visibility,
                //     name: *name,
                //     typ: ty.clone(),
                //     is_const: *is_const,
                //     index,
                //     local_index: 0,
                // };
                // locals.push(local);
                // scope.insert(*name, decl::Decl::Local(index));
                // Ok(out
                //     .alloc(Expr::new(
                //         ast.location,
                //         ExprKind::LocalDecl(index, value_expr),
                //     ))
                //     .with_type(ExprType::Void))
            }
            _ => todo!("Decl: {:?}", decl),
        },

        NodeKind::Block(stmts, result) => {
            let block_parent_scope = Scope {
                parent: Some(parent_scope),
                decls: current_scope,
            };

            build_block(
                ast,
                host,
                &block_parent_scope,
                locals,
                inference,
                out,
                stmts,
                result,
            )
        }

        NodeKind::Cast { arg, typ } => {
            let mut infer = TypeInference::default();
            let to_typ = resolve_types(parent_scope, typ)?;
            let arg_expr = build_exprs(
                arg,
                host,
                parent_scope,
                current_scope,
                locals,
                &mut infer,
                out,
            )?;
            infer.solve_constraints()?;

            if to_typ == arg_expr.typ {
                Ok(arg_expr)
            } else if to_typ.is_number() && arg_expr.typ.is_number() {
                assign_types(arg_expr, &infer)?;
                Ok(out
                    .alloc(Expr::new(ast.location, ExprKind::Cast(arg_expr)))
                    .with_type(to_typ))
            } else {
                Err(CompilationError::InvalidCast(
                    ast.location,
                    to_typ.clone(),
                    arg_expr.typ.clone(),
                ))
            }
        }

        NodeKind::Call(func, args) => {
            let func_expr = build_exprs(
                func,
                host,
                parent_scope,
                current_scope,
                locals,
                inference,
                out,
            )?;
            let mut arg_exprs = Vec::<&mut Expr>::new();
            for arg in args.iter() {
                let arg_expr = build_exprs(
                    arg,
                    host,
                    parent_scope,
                    current_scope,
                    locals,
                    inference,
                    out,
                )?;
                arg_exprs.push(arg_expr);
            }

            // let ret_type = inference.fresh_typevar();
            let fty = match func_expr.typ.clone() {
                ExprType::Function(fty) => fty.clone(),
                _ => {
                    return Err(CompilationError::NotCallable(ast.location));
                }
            };

            if fty.params.len() != arg_exprs.len() {
                return Err(CompilationError::IncorrectNumberOfArguments(
                    ast.location,
                    fty.params.len(),
                    arg_exprs.len(),
                ));
            }

            for (param, arg) in fty.params.iter().zip(arg_exprs.iter()) {
                inference.add_constraint(param.typ.clone(), arg.typ.clone(), arg.location);
            }

            // inference.add_constraint(ret_type.clone(), fty.ret.clone(), ast.location);
            // inference.solve_constraints()?;
            for arg in arg_exprs.iter_mut() {
                assign_types(arg, inference)?;
            }

            let call = ExprKind::Call(func_expr, arg_exprs);
            Ok(out
                .alloc(Expr::new(ast.location, call))
                .with_type(fty.ret.clone()))
        }

        NodeKind::If {
            test,
            then_block,
            else_block,
        } => {
            let test_expr = build_exprs(
                test,
                host,
                parent_scope,
                current_scope,
                locals,
                inference,
                out,
            )?;
            inference.add_constraint(ExprType::Boolean, test_expr.typ.clone(), test.location);

            let then_branch = build_exprs(
                then_block,
                host,
                parent_scope,
                current_scope,
                locals,
                inference,
                out,
            )?;

            if let Some(else_block) = else_block {
                let else_branch = build_exprs(
                    else_block,
                    host,
                    parent_scope,
                    current_scope,
                    locals,
                    inference,
                    out,
                )?;
                let ty = inference.fresh_typevar();
                // Both sides must be the same, which is also the result type.
                inference.add_constraint(ty.clone(), then_branch.typ.clone(), then_block.location);
                inference.add_constraint(ty.clone(), else_branch.typ.clone(), else_block.location);
                Ok(out
                    .alloc(Expr::new(
                        ast.location,
                        ExprKind::If {
                            test: test_expr,
                            then_branch,
                            else_branch: Some(else_branch),
                        },
                    ))
                    .with_type(ty))
            } else {
                // If there's no else branch, then the then branch must not return anything
                inference.add_constraint(
                    ExprType::Void,
                    then_branch.typ.clone(),
                    then_block.location,
                );
                Ok(out
                    .alloc(Expr::new(
                        ast.location,
                        ExprKind::If {
                            test: test_expr,
                            then_branch,
                            else_branch: None,
                        },
                    ))
                    .with_type(ExprType::Void))
            }
        }

        _ => {
            panic!("Invalid AST node for expression: {:?}", ast.kind);
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn build_block<'a, 'e>(
    ast: &'a ASTNode<'a>,
    host: &HostState,
    parent_scope: &decl::Scope,
    locals: &mut usize,
    inference: &mut TypeInference,
    out: &'e bumpalo::Bump,
    stmts: &[&ASTNode<'_>],
    result: &Option<&ASTNode<'_>>,
) -> Result<&'e mut Expr<'e>, CompilationError> {
    let mut stmt_exprs = Vec::<&mut Expr>::new();

    let mut local_decls = DeclTable::default();
    for stmt in stmts {
        let stmt_expr = build_exprs(
            stmt,
            host,
            parent_scope,
            &mut local_decls,
            locals,
            inference,
            out,
        )?;
        stmt_exprs.push(stmt_expr);
    }

    let result_expr = match result {
        Some(result) => Some(build_exprs(
            result,
            host,
            parent_scope,
            &mut local_decls,
            locals,
            inference,
            out,
        )?),
        None => None,
    };
    let result_type = result_expr
        .as_ref()
        .map(|expr| expr.typ.clone())
        .unwrap_or(ExprType::Void);
    let location = result_expr
        .as_ref()
        .map(|expr| expr.location)
        .unwrap_or(ast.location);
    Ok(out
        .alloc(Expr::new(
            location,
            ExprKind::Block(stmt_exprs, result_expr),
        ))
        .with_type(result_type))
}
#[cfg(test)]
mod tests {
    use crate::location::TokenLocation;

    use super::*;

    #[test]
    fn test_build_exprs_lit_int_unsized() {
        let arena = bumpalo::Bump::new();
        let ast = ASTNode {
            location: TokenLocation::default(),
            kind: NodeKind::LitInt(42, IntegerSuffix::Unsized),
        };
        let host = HostState::default();
        let scope = Scope {
            parent: None,
            decls: &DeclTable::default(),
        };
        let mut current_scope = DeclTable::default();
        let mut locals = 0;
        let mut inference = TypeInference::default();

        let result = build_exprs(
            &ast,
            &host,
            &scope,
            &mut current_scope,
            &mut locals,
            &mut inference,
            &arena,
        );
        assert!(result.is_ok());
        let expr = result.unwrap();
        assert!(matches!(expr.kind, ExprKind::ConstInteger(42)));
    }

    #[test]
    fn test_build_exprs_lit_int_i32() {
        let arena = bumpalo::Bump::new();
        let ast = ASTNode {
            location: TokenLocation::default(),
            kind: NodeKind::LitInt(100, IntegerSuffix::I32),
        };
        let host = HostState::default();
        let scope = Scope {
            parent: None,
            decls: &DeclTable::default(),
        };
        let mut current_scope = DeclTable::default();
        let mut locals = 0;
        let mut inference = TypeInference::default();

        let result = build_exprs(
            &ast,
            &host,
            &scope,
            &mut current_scope,
            &mut locals,
            &mut inference,
            &arena,
        );
        assert!(result.is_ok());
        let expr = result.unwrap();
        assert_eq!(expr.typ, ExprType::I32);
    }

    #[test]
    fn test_build_exprs_lit_float() {
        let arena = bumpalo::Bump::new();
        let ast = ASTNode {
            location: TokenLocation::default(),
            kind: NodeKind::LitFloat(3.1, FloatSuffix::F32),
        };
        let host = HostState::default();
        let scope = Scope {
            parent: None,
            decls: &DeclTable::default(),
        };
        let mut current_scope = DeclTable::default();
        let mut locals = 0;
        let mut inference = TypeInference::default();

        let result = build_exprs(
            &ast,
            &host,
            &scope,
            &mut current_scope,
            &mut locals,
            &mut inference,
            &arena,
        );
        assert!(result.is_ok());
        let expr = result.unwrap();
        assert!(matches!(expr.kind, ExprKind::ConstFloat(_)));
        assert_eq!(expr.typ, ExprType::F32);
    }

    #[test]
    fn test_build_exprs_lit_bool() {
        let arena = bumpalo::Bump::new();
        let ast = ASTNode {
            location: TokenLocation::default(),
            kind: NodeKind::LitBool(true),
        };
        let host = HostState::default();
        let scope = Scope {
            parent: None,
            decls: &DeclTable::default(),
        };
        let mut current_scope = DeclTable::default();
        let mut locals = 0;
        let mut inference = TypeInference::default();

        let result = build_exprs(
            &ast,
            &host,
            &scope,
            &mut current_scope,
            &mut locals,
            &mut inference,
            &arena,
        );
        assert!(result.is_ok());
        let expr = result.unwrap();
        assert!(matches!(expr.kind, ExprKind::ConstBool(true)));
        assert_eq!(expr.typ, ExprType::Boolean);
    }

    #[test]
    fn test_build_exprs_lit_string() {
        let arena = bumpalo::Bump::new();
        let ast = ASTNode {
            location: TokenLocation::default(),
            kind: NodeKind::LitString("hello".into()),
        };
        let host = HostState::default();
        let scope = Scope {
            parent: None,
            decls: &DeclTable::default(),
        };
        let mut current_scope = DeclTable::default();
        let mut locals = 0;
        let mut inference = TypeInference::default();

        let result = build_exprs(
            &ast,
            &host,
            &scope,
            &mut current_scope,
            &mut locals,
            &mut inference,
            &arena,
        );
        assert!(result.is_ok());
        let expr = result.unwrap();
        assert_eq!(expr.typ, ExprType::String);
    }

    #[test]
    fn test_build_exprs_empty() {
        let arena = bumpalo::Bump::new();
        let ast = ASTNode {
            location: TokenLocation::default(),
            kind: NodeKind::Empty,
        };
        let host = HostState::default();
        let scope = Scope {
            parent: None,
            decls: &DeclTable::default(),
        };
        let mut current_scope = DeclTable::default();
        let mut locals = 0;
        let mut inference = TypeInference::default();

        let result = build_exprs(
            &ast,
            &host,
            &scope,
            &mut current_scope,
            &mut locals,
            &mut inference,
            &arena,
        );
        assert!(result.is_ok());
        let expr = result.unwrap();
        assert!(matches!(expr.kind, ExprKind::Empty));
    }

    #[test]
    fn test_build_exprs_unknown_ident() {
        let arena = bumpalo::Bump::new();
        let ast = ASTNode {
            location: TokenLocation::default(),
            kind: NodeKind::Ident("unknown".into()),
        };
        let host = HostState::default();
        let scope = Scope {
            parent: None,
            decls: &DeclTable::default(),
        };
        let mut current_scope = DeclTable::default();
        let mut locals = 0;
        let mut inference = TypeInference::default();

        let result = build_exprs(
            &ast,
            &host,
            &scope,
            &mut current_scope,
            &mut locals,
            &mut inference,
            &arena,
        );
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err(),
            CompilationError::UnknownSymbol(_, _)
        ));
    }

    #[test]
    fn test_build_exprs_unary_not() {
        let arena = bumpalo::Bump::new();
        let arg_ast = arena.alloc(ASTNode {
            location: TokenLocation::default(),
            kind: NodeKind::LitBool(true),
        });
        let ast = ASTNode {
            location: TokenLocation::default(),
            kind: NodeKind::UnaryExpr {
                op: UnaryOp::Not,
                arg: arg_ast,
            },
        };
        let host = HostState::default();
        let scope = Scope {
            parent: None,
            decls: &DeclTable::default(),
        };
        let mut current_scope = DeclTable::default();
        let mut locals = 0;
        let mut inference = TypeInference::default();

        let result = build_exprs(
            &ast,
            &host,
            &scope,
            &mut current_scope,
            &mut locals,
            &mut inference,
            &arena,
        );
        assert!(result.is_ok());
        let expr = result.unwrap();
        assert!(matches!(
            expr.kind,
            ExprKind::UnaryExpr {
                op: UnaryOp::Not,
                ..
            }
        ));
        assert_eq!(expr.typ, ExprType::Boolean);
    }

    #[test]
    fn test_build_exprs_binary_add() {
        let arena = bumpalo::Bump::new();
        let lhs_ast = arena.alloc(ASTNode {
            location: TokenLocation::default(),
            kind: NodeKind::LitInt(1, IntegerSuffix::I32),
        });
        let rhs_ast = arena.alloc(ASTNode {
            location: TokenLocation::default(),
            kind: NodeKind::LitInt(2, IntegerSuffix::I32),
        });
        let ast = ASTNode {
            location: TokenLocation::default(),
            kind: NodeKind::BinaryExpr {
                op: BinaryOp::Add,
                lhs: lhs_ast,
                rhs: rhs_ast,
            },
        };
        let host = HostState::default();
        let scope = Scope {
            parent: None,
            decls: &DeclTable::default(),
        };
        let mut current_scope = DeclTable::default();
        let mut locals = 0;
        let mut inference = TypeInference::default();

        let result = build_exprs(
            &ast,
            &host,
            &scope,
            &mut current_scope,
            &mut locals,
            &mut inference,
            &arena,
        );
        assert!(result.is_ok());
        let expr = result.unwrap();
        assert!(matches!(
            expr.kind,
            ExprKind::BinaryExpr {
                op: BinaryOp::Add,
                ..
            }
        ));
    }

    #[test]
    fn test_build_exprs_binary_logical_and() {
        let arena = bumpalo::Bump::new();
        let lhs_ast = arena.alloc(ASTNode {
            location: TokenLocation::default(),
            kind: NodeKind::LitBool(true),
        });
        let rhs_ast = arena.alloc(ASTNode {
            location: TokenLocation::default(),
            kind: NodeKind::LitBool(false),
        });
        let ast = ASTNode {
            location: TokenLocation::default(),
            kind: NodeKind::BinaryExpr {
                op: BinaryOp::LogAnd,
                lhs: lhs_ast,
                rhs: rhs_ast,
            },
        };
        let host = HostState::default();
        let scope = Scope {
            parent: None,
            decls: &DeclTable::default(),
        };
        let mut current_scope = DeclTable::default();
        let mut locals = 0;
        let mut inference = TypeInference::default();

        let result = build_exprs(
            &ast,
            &host,
            &scope,
            &mut current_scope,
            &mut locals,
            &mut inference,
            &arena,
        );
        assert!(result.is_ok());
        let expr = result.unwrap();
        assert_eq!(expr.typ, ExprType::Boolean);
    }

    #[test]
    fn test_build_exprs_block_with_trailing_expr() {
        let arena = bumpalo::Bump::new();

        // Create a simple statement: let x = 5;
        let stmt_ast: &ASTNode = arena.alloc(ASTNode {
            location: TokenLocation::default(),
            kind: NodeKind::Empty,
        });

        // Trailing expression: 42
        let result_ast = arena.alloc(ASTNode {
            location: TokenLocation::default(),
            kind: NodeKind::LitInt(42, IntegerSuffix::I32),
        });

        let ast = ASTNode {
            location: TokenLocation::default(),
            kind: NodeKind::Block(arena.alloc_slice_copy(&[stmt_ast]), Some(result_ast)),
        };

        let host = HostState::default();
        let scope = Scope {
            parent: None,
            decls: &DeclTable::default(),
        };
        let mut current_scope = DeclTable::default();
        let mut locals = 0;
        let mut inference = TypeInference::default();

        let result = build_exprs(
            &ast,
            &host,
            &scope,
            &mut current_scope,
            &mut locals,
            &mut inference,
            &arena,
        );

        assert!(result.is_ok());
        let expr = result.unwrap();
        assert!(matches!(expr.kind, ExprKind::Block(_, Some(_))));
        assert_eq!(expr.typ, ExprType::I32);
    }

    #[test]
    fn test_build_exprs_block_without_trailing_expr() {
        let arena = bumpalo::Bump::new();

        // Create a simple statement
        let stmt_ast: &ASTNode = arena.alloc(ASTNode {
            location: TokenLocation::default(),
            kind: NodeKind::Empty,
        });

        let ast = ASTNode {
            location: TokenLocation::default(),
            kind: NodeKind::Block(arena.alloc_slice_copy(&[stmt_ast]), None),
        };

        let host = HostState::default();
        let scope = Scope {
            parent: None,
            decls: &DeclTable::default(),
        };
        let mut current_scope = DeclTable::default();
        let mut locals = 0;
        let mut inference = TypeInference::default();

        let result = build_exprs(
            &ast,
            &host,
            &scope,
            &mut current_scope,
            &mut locals,
            &mut inference,
            &arena,
        );

        assert!(result.is_ok());
        let expr = result.unwrap();
        assert!(matches!(expr.kind, ExprKind::Block(_, None)));
        assert_eq!(expr.typ, ExprType::Void);
    }

    #[test]
    fn test_build_module_exprs_with_function() {
        let arena = bumpalo::Bump::new();
        let mut module = Module::default();
        let host = HostState::new();
        let expr_arena = bumpalo::Bump::new();

        // Create a simple function: fn test() -> i32 { 42 }
        let body_ast = arena.alloc(ASTNode {
            location: TokenLocation::default(),
            kind: NodeKind::LitInt(42, IntegerSuffix::I32),
        });

        let ret_type_ast = arena.alloc(ASTNode {
            location: TokenLocation::default(),
            kind: NodeKind::Ident("i32".into()),
        });

        let func_ast = arena.alloc(ASTNode {
            location: TokenLocation::default(),
            kind: NodeKind::Decl(arena.alloc(ASTDecl::Function {
                name: "test".into(),
                visibility: DeclVisibility::Public,
                params: arena.alloc_slice_copy(&[]),
                ret: Some(ret_type_ast),
                body: Some(body_ast),
                is_native: false,
            })),
        });

        let module_ast = ASTNode {
            location: TokenLocation::default(),
            kind: NodeKind::Module(arena.alloc_slice_copy(&[&*func_ast])),
        };

        let result = build_module_exprs(&host, &mut module, &module_ast, &expr_arena);

        let module_exprs = result.unwrap();
        assert_eq!(module_exprs.functions.len(), 1);
        assert!(module.module_decls.contains_key("test"));
    }

    #[test]
    fn test_build_module_exprs_function_redefinition() {
        let arena = bumpalo::Bump::new();
        let mut module = Module::default();
        let host = HostState::default();
        let expr_arena = bumpalo::Bump::new();

        let func_ast = arena.alloc(ASTNode {
            location: TokenLocation::default(),
            kind: NodeKind::Decl(arena.alloc(ASTDecl::Function {
                name: "test".into(),
                visibility: DeclVisibility::Public,
                params: arena.alloc_slice_copy(&[]),
                ret: None,
                body: None,
                is_native: false,
            })),
        });

        let module_ast = ASTNode {
            location: TokenLocation::default(),
            kind: NodeKind::Module(arena.alloc_slice_copy(&[&*func_ast, &*func_ast])),
        };

        let result = build_module_exprs(&host, &mut module, &module_ast, &expr_arena);

        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err(),
            CompilationError::FunctionRedefinition(_, _)
        ));
    }
}
