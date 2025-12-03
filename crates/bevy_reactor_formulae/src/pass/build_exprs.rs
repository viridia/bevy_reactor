use core::result;
use std::sync::Arc;

use bevy::{render::render_graph::Node, scene::ron::de};

use crate::{
    ast::{ASTNode, NodeKind},
    compiler::{
        self, CompilationError, CompilationUnit, CompiledFunction, CompiledModule, FunctionBody,
        ModuleExprs,
    },
    decl::{self, DeclKind, DeclTable, DeclVisibility, ParamDecl, Scope},
    expr::{Expr, ExprKind},
    expr_type::{ExprType, FunctionType},
    host::HostState,
    oper::{BinaryOp, UnaryOp},
};

use super::{
    assign_types::assign_types, resolve_types::resolve_types, type_inference::TypeInference,
};

pub(crate) fn build_module_decls<'ast>(
    _host: &HostState,
    module: &mut CompiledModule,
    ast: &'ast ASTNode<'ast>,
) -> Result<(), CompilationError> {
    if let NodeKind::Program(ast_decls) = &ast.kind {
        for ast_decl in *ast_decls {
            match &ast_decl.kind {
                // NodeKind::Import(_, _) => {}
                NodeKind::Decl(d) => match d {
                    crate::ast::ASTDecl::Function {
                        name,
                        visibility,
                        is_native: _,
                        ..
                    } => {
                        // Multiple declarations of the same function are not allowed.
                        if module.module_decls.contains_key(name) {
                            return Err(CompilationError::FunctionRedefinition(
                                ast_decl.location,
                                name.to_string(),
                            ));
                        }

                        let fd = decl::Decl {
                            location: ast_decl.location,
                            name: name.clone(),
                            visibility: *visibility,
                            kind: DeclKind::Function {
                                params: Vec::new(),
                                ret: ExprType::default(),
                                is_native: false,
                                index: 0,
                            },
                        };

                        module.module_decls.insert(name.clone(), fd);
                        // let findex = module.add_function(fd);
                        // scope.insert(*name, decl::Decl::Function(findex));
                    }
                    crate::ast::ASTDecl::Let {
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
                    } // crate::ast::ASTDecl::Struct {
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
                      // } // crate::ast::ASTDecl::TypeAlias { .. } => todo!(),
                },
                _ => panic!("Invalid AST node for declaration: {:?}", ast_decl.kind),
            }
        }

        // Assign indices to functions. Imported functions first.
        // let mut index = 0;
        // for fd in decls.functions.iter_mut() {
        //     if fd.is_native {
        //         fd.function_index = index;
        //         index += 1;
        //     }
        // }

        // // Then local functions.
        // for fd in decls.functions.iter_mut() {
        //     if !fd.is_native {
        //         fd.function_index = index;
        //         index += 1;
        //     }
        // }

        Ok(())
    } else {
        panic!("Invalid AST node for compilation unit: {:?}", ast.kind);
    }
}

pub(crate) fn build_module_exprs<'ast>(
    host: &HostState,
    module: &mut CompiledModule,
    ast: &'ast ASTNode<'ast>,
) -> Result<(), CompilationError> {
    // Resolve types for all declarations.
    if let NodeKind::Program(ast_decls) = &ast.kind {
        let host_scope = Scope {
            parent: None,
            decls: &host.global_decls,
        };
        let module_scope = Scope {
            parent: Some(&host_scope),
            decls: &module.module_decls,
        };

        for ast_decl in *ast_decls {
            match &ast_decl.kind {
                // NodeKind::Import(_, _) => {}
                NodeKind::Decl(d) => match d {
                    crate::ast::ASTDecl::Function {
                        name,
                        params,
                        ret: ret_ast,
                        ..
                    } => {
                        let decl = module_scope.get(name).unwrap();
                        let ret_type = match ret_ast {
                            None => ExprType::Void,
                            Some(ret_ast) => resolve_types(&module_scope, ret_ast)?,
                        };

                        let mut params_mapped: Vec<ParamDecl> = Vec::with_capacity(params.len());
                        for (i, p) in params.iter().enumerate() {
                            let typ = resolve_types(&module_scope, p.typ)?;
                            params_mapped.push(ParamDecl {
                                location: p.location,
                                name: p.name.clone(),
                                typ,
                                index: i,
                                local_index: 0,
                            });
                        }

                        todo!();

                        // let decl::Decl::Function(findex) = decl else {
                        //     unreachable!()
                        // };
                        // decls.functions[*findex].typ = Arc::new(FunctionType {
                        //     params: params_mapped,
                        //     ret: ret_type,
                        // });
                    }
                    crate::ast::ASTDecl::Let { .. } => todo!(),
                    // crate::ast::ASTDecl::Struct {
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
                    // } // crate::ast::ASTDecl::TypeAlias { .. } => todo!(),
                },
                _ => panic!("Invalid AST node for declaration: {:?}", ast_decl.kind),
            }
        }

        // Build function body expressions.
        for (ref name, ref mut decl) in module.module_decls.iter_mut() {}
        // for decl_ast in *ast_decls {
        //     if let NodeKind::Decl(crate::ast::ASTDecl::Function {
        //         name,
        //         body: body_ast,
        //         ..
        //     }) = &decl_ast.kind
        //     {
        //         let decl = scope.get(*name).unwrap();
        //         let decl::Decl::Function(findex) = decl else {
        //             unreachable!()
        //         };

        //         let mut inference: TypeInference = Default::default();
        //         let mut param_scope = Scope::new(Some(scope));
        //         let function = &mut decls.functions[*findex];
        //         for param in function.typ.params.iter() {
        //             param_scope.insert(
        //                 param.name,
        //                 decl::Decl::Param(param.typ.clone(), param.index),
        //             );
        //         }
        //         let mut local_scope = Scope::new(Some(&param_scope));
        //         let mut locals_table: Vec<LocalDecl> = Vec::new();
        //         let mut body_expr = if let Some(body_ast) = body_ast {
        //             if function.is_native {
        //                 return Err(CompilationError::NativeFunctionHasBody(function.location));
        //             }
        //             build_exprs(
        //                 body_ast,
        //                 &mut local_scope,
        //                 decls,
        //                 &mut locals_table,
        //                 &mut inference,
        //             )?
        //         } else {
        //             if !function.is_native {
        //                 return Err(CompilationError::MissingBody(function.location));
        //             }
        //             Expr::new(decl_ast.location, ExprKind::Empty)
        //         };
        //         let function = &mut decls.functions[*findex];
        //         if body_ast.is_some() {
        //             inference.add_constraint(
        //                 function.typ.ret.clone(),
        //                 body_expr.typ.clone(),
        //                 body_expr.location,
        //             );
        //             inference.solve_constraints()?;
        //             assign_types(&mut body_expr, &inference)?;
        //             for local in locals_table.iter_mut() {
        //                 local.typ = inference.substitute(&local.typ);
        //             }
        //         }
        //         function.body = body_expr;
        //         function.locals = locals_table;
        //     }
        // }

        Ok(())
    } else {
        panic!("Invalid AST node for compilation unit: {:?}", ast.kind);
    }
}

pub(crate) fn build_formula_exprs<'ast, 'me>(
    host: &HostState,
    module: &mut CompiledModule,
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
        // let module_scope = Scope {
        //     parent: Some(&host_scope),
        //     decls: &module.module_decls,
        // };

        // Multiple declarations of the same function are not allowed.
        if module.module_decls.contains_key(CompiledModule::DEFAULT) {
            // This should never happen since this is only called once per compilation unit.
            return Err(CompilationError::FunctionRedefinition(
                ast.location,
                CompiledModule::DEFAULT.to_string(),
            ));
        }

        let function_index = module_exprs.functions.len();
        let fd = decl::Decl {
            location: ast.location,
            name: CompiledModule::DEFAULT.into(),
            visibility: DeclVisibility::Private,
            kind: DeclKind::Function {
                params: Vec::new(),
                ret: ret_type.clone(),
                is_native: false,
                index: function_index,
            },
        };
        module_exprs.functions.push(FunctionBody { body: None });

        // module.functions.push(CompiledFunction {
        //     code: todo!(),
        //     locals: todo!(),
        // });

        let mut inference: TypeInference = Default::default();
        // let param_decls = DeclTable::new();
        // let param_scope = Scope {
        //     parent: Some(&scope),
        //     decls: &param_decls,
        // };

        // let param_scope = Scope::new(Some(scope));
        // let mut local_scope = Scope::new(Some(&param_scope));
        let mut local_index: usize = 0;
        // let mut locals_table: Vec<LocalDecl> = Vec::new();
        let body_expr = build_exprs(
            ast,
            &host_scope,
            &mut module.module_decls,
            // decls,
            &mut local_index,
            &mut inference,
            expr_arena,
        )?;

        inference.add_constraint(ret_type.clone(), body_expr.typ.clone(), body_expr.location);
        inference.solve_constraints()?;
        assign_types(body_expr, &inference)?;
        module_exprs.functions[function_index].body = Some(body_expr);

        // for local in locals_table.iter_mut() {
        //     local.typ = inference.substitute(&local.typ);
        // }

        // fd.body = Some(body_expr);
        // fd.locals = locals_table;

        module
            .module_decls
            .insert(CompiledModule::DEFAULT.into(), fd);

        // let findex = decls.add_function(fd);
        // module_scope.insert(name, decl::Decl::Function(findex));

        for (ref name, ref mut decl) in module.module_decls.iter_mut() {}

        Ok(module_exprs)
    } else {
        panic!("Invalid AST node for formula: {:?}", ast.kind);
    }
}

pub(crate) fn build_exprs<'a, 'e>(
    ast: &'a ASTNode<'a>,
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
                        params,
                        ret,
                        is_native,
                        index,
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
                        is_const,
                        index,
                    } => Ok(out
                        .alloc(Expr::new(ast.location, ExprKind::LocalRef(*index)))
                        .with_type(typ.clone())),
                    decl::DeclKind::Global {
                        typ,
                        is_const,
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
            let lhs_expr = build_exprs(lhs, parent_scope, current_scope, locals, inference, out)?;
            let rhs_expr = build_exprs(rhs, parent_scope, current_scope, locals, inference, out)?;
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

                BinaryOp::Eq | BinaryOp::Ne => todo!(),

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
            let lhs_expr = build_exprs(lhs, parent_scope, current_scope, locals, inference, out)?;
            let rhs_expr = build_exprs(rhs, parent_scope, current_scope, locals, inference, out)?;

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
            let lhs_expr = build_exprs(lhs, parent_scope, current_scope, locals, inference, out)?;
            let rhs_expr = build_exprs(rhs, parent_scope, current_scope, locals, inference, out)?;
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
            let arg_expr = build_exprs(arg, parent_scope, current_scope, locals, inference, out)?;
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
            let base_expr = build_exprs(base, parent_scope, current_scope, locals, inference, out)?;
            match base_expr.typ.clone() {
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

        NodeKind::FieldIndex(base, index) => {
            let base_expr = build_exprs(base, parent_scope, current_scope, locals, inference, out)?;
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
            crate::ast::ASTDecl::Let {
                name,
                typ,
                value,
                is_const,
                visibility,
                ..
            } => {
                let typ = match typ {
                    Some(typ) => Some(resolve_types(parent_scope, typ)?),
                    None => None,
                };
                let value_expr = match value {
                    Some(value) => Some(build_exprs(
                        value,
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
            let mut stmt_exprs = Vec::<&mut Expr>::new();
            for stmt in *stmts {
                let stmt_expr =
                    build_exprs(stmt, parent_scope, current_scope, locals, inference, out)?;
                stmt_exprs.push(stmt_expr);
            }

            let result_expr = match result {
                Some(result) => Some(build_exprs(
                    result,
                    parent_scope,
                    current_scope,
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

        NodeKind::Cast { arg, typ } => {
            let mut infer = TypeInference::default();
            let to_typ = resolve_types(parent_scope, typ)?;
            let arg_expr = build_exprs(arg, parent_scope, current_scope, locals, &mut infer, out)?;
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
            let func_expr = build_exprs(func, parent_scope, current_scope, locals, inference, out)?;
            let mut arg_exprs = Vec::<&mut Expr>::new();
            for arg in args.iter() {
                let arg_expr =
                    build_exprs(arg, parent_scope, current_scope, locals, inference, out)?;
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

        _ => {
            panic!("Invalid AST node for expression: {:?}", ast.kind);
        }
    }
}
