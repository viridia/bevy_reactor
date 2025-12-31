use core::result;
use std::sync::Arc;

use crate::Module;
use crate::ast::{ASTNode, FloatSuffix, IntegerSuffix, NodeKind};
use crate::decl::{Decl, DeclKind, DeclTable, Scope, ScopeType};
use crate::expr_type::ExprType;
use crate::host::HostState;
use crate::pass::build_exprs;
use bevy::ecs::entity::Entity;
use bevy::log::tracing_subscriber::field;
use bevy::reflect::TypeInfo;
use bevy::{log::info, render::render_graph::Node, scene::ron::de};
use smol_str::SmolStr;

use crate::{
    ast::ASTDecl,
    compiler::{self, CompilationError, CompiledFunction, ModuleExprs},
    decl::{self, DeclVisibility, FunctionParam},
    expr::{Expr, ExprKind, FunctionBody},
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
            decls: &host.decls,
            scope_type: ScopeType::Host,
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
            typ: ExprType::Function(Arc::new(FunctionType {
                params: Vec::new(),
                ret: ret_type.clone(),
            })),
            kind: DeclKind::Function {
                index: function_index,
            },
        };
        let mut function = FunctionBody {
            body: None,
            locals: Vec::new(),
            num_params: 0,
        };

        let mut inference: TypeInference = Default::default();

        // Scope for the whole module.
        let module_scope = Scope {
            parent: Some(&host_scope),
            decls: &module.module_decls,
            scope_type: ScopeType::Module,
        };

        // Scope for the function parameters from within the function body.
        let param_decls = DeclTable::default();
        let param_scope = Scope {
            parent: Some(&module_scope),
            decls: &param_decls,
            scope_type: ScopeType::Param,
        };

        // let function = &mut module_exprs.functions[function_index];
        let body_expr = build_block(
            ast,
            host,
            &param_scope,
            &mut function,
            &mut inference,
            expr_arena,
            &non_hoisted,
            result,
        )?;

        inference.add_constraint(ret_type.clone(), body_expr.typ.clone(), body_expr.location);
        inference.solve_constraints()?;
        assign_types(body_expr, &inference)?;
        function.body = Some(body_expr);

        for local_type in function.locals.iter_mut() {
            inference.replace_type_vars(local_type);
        }

        module_exprs.functions.push(function);
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
                        typ: ExprType::None,
                        kind: DeclKind::Function {
                            index: function_index,
                        },
                    };
                    module_exprs.functions.push(FunctionBody {
                        body: None,
                        locals: Vec::new(),
                        num_params: 0,
                    });
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
                }
                ASTDecl::Struct {
                    name, visibility, ..
                } => {
                    // Multiple declarations of the same function are not allowed.
                    if module.module_decls.contains_key(name) {
                        return Err(CompilationError::NameRedefinition(
                            ast_decl.location,
                            name.to_string(),
                        ));
                    }

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
                    todo!("{name} {visibility:?}");
                } // } // ASTDecl::TypeAlias { .. } => todo!(),
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
        decls: &host.decls,
        scope_type: ScopeType::Host,
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
                        scope_type: ScopeType::Module,
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
                        });
                    }

                    let decl = module.module_decls.get_mut(name).unwrap();
                    decl.typ = ExprType::Function(Arc::new(FunctionType {
                        params: params_mapped,
                        ret: ret_type,
                    }));
                }

                ASTDecl::Let { .. } => todo!(),
                ASTDecl::Struct { name, fields, .. } => {
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
                    todo!("Struct {name} {fields:?}");
                } // ASTDecl::TypeAlias { .. } => todo!(),
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
        decls: &host.decls,
        scope_type: ScopeType::Host,
    };

    for ast_decl in ast_decls {
        if let NodeKind::Decl(d) = ast_decl.kind
            && let ASTDecl::Function { name, body, .. } = d
        {
            let decl = module.module_decls.get_mut(name).unwrap();
            let DeclKind::Function {
                index: fd_index, ..
            } = &mut decl.kind
            else {
                panic!("Expected function declaration");
            };

            let ExprType::Function(ftype) = &decl.typ else {
                panic!("Expected function type");
            };
            let fd_ret = ftype.ret.clone();
            let fd_index = *fd_index;

            let Some(body_ast) = body else {
                continue;
            };

            // Build parameter scope
            let mut param_decls = DeclTable::default();
            for param in ftype.params.iter() {
                param_decls.insert(
                    param.name.clone(),
                    Decl {
                        location: param.location,
                        visibility: DeclVisibility::Public,
                        typ: param.typ.clone(),
                        kind: DeclKind::Param { index: param.index },
                    },
                );
            }

            let module_scope = Scope {
                parent: Some(&host_scope),
                decls: &module.module_decls,
                scope_type: ScopeType::Module,
            };

            let param_scope = Scope {
                parent: Some(&module_scope),
                decls: &param_decls,
                scope_type: ScopeType::Param,
            };

            let mut inference: TypeInference = Default::default();
            // let mut local_index: usize = 0;
            let function = &mut module_exprs.functions[fd_index];
            let body_expr = build_exprs(
                body_ast,
                host,
                &param_scope,
                function,
                &mut inference,
                expr_arena,
            )?;

            inference.add_constraint(fd_ret, body_expr.typ.clone(), body_expr.location);
            inference.solve_constraints()?;
            assign_types(body_expr, &inference)?;
            for local_type in function.locals.iter_mut() {
                inference.replace_type_vars(local_type);
            }
            function.body = Some(body_expr);
            function.num_params = param_decls.len();
            // fb.num_locals = local_index;
        }
    }

    Ok(())
}

fn build_exprs<'a, 'e>(
    ast: &'a ASTNode<'a>,
    host: &HostState,
    scope: &decl::Scope,
    function: &mut FunctionBody,
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

        NodeKind::Ident(name) => {
            match scope.lookup(name) {
                Some((scope_type, decl)) => {
                    match &decl.kind {
                        decl::DeclKind::Function { index } => match scope_type {
                            ScopeType::Host => Ok(out
                                .alloc(Expr::new(ast.location, ExprKind::HostFunctionRef(*index)))
                                .with_type(decl.typ.clone())),
                            ScopeType::Module => Ok(out
                                .alloc(Expr::new(ast.location, ExprKind::ScriptFunctionRef(*index)))
                                .with_type(decl.typ.clone())),
                            ScopeType::Param => todo!(),
                            ScopeType::Local => todo!(),
                        },

                        decl::DeclKind::Local { is_const: _, index } => Ok(out
                            .alloc(Expr::new(ast.location, ExprKind::LocalRef(*index)))
                            .with_type(decl.typ.clone())),
                        decl::DeclKind::Global { is_const: _, index } => Ok(out
                            .alloc(Expr::new(ast.location, ExprKind::GlobalRef(*index)))
                            .with_type(decl.typ.clone())),
                        decl::DeclKind::Param { index } => Ok(out
                            .alloc(Expr::new(ast.location, ExprKind::ParamRef(*index)))
                            .with_type(decl.typ.clone())),
                        // Struct (constructor)
                        // Enum (constructor)
                        _ => todo!("Ident: {:?}", decl),
                    }
                }
                None => Err(CompilationError::UnknownSymbol(
                    ast.location,
                    name.to_string(),
                )),
            }
        }

        NodeKind::QName(name_parts) => {
            assert!(name_parts.len() > 1);
            let (head, tail) = name_parts.split_at(1);

            let NodeKind::Ident(ref head_name) = head[0].kind else {
                panic!("Expected QName head to be a name");
            };

            match scope.lookup(head_name) {
                Some((_scope_type, decl)) => match &decl.kind {
                    decl::DeclKind::NativeType { id } => {
                        let native_type = host.get_host_type_by_id(*id).unwrap();
                        // Look for inner name.
                        // TODO: Make this recursive.
                        let NodeKind::Ident(ref fname) = tail[0].kind else {
                            panic!("Expected QName element to be a name");
                        };
                        let Some(member_decl) = native_type.decls.get(fname) else {
                            return Err(CompilationError::UnknownField(
                                ast.location,
                                head_name.clone(),
                                fname.clone(),
                            ));
                        };
                        match member_decl.kind {
                            DeclKind::Global { .. } => todo!(),
                            DeclKind::Local { .. } => todo!(),
                            DeclKind::Param { .. } => {
                                unreachable!("Parameter name can't be qualified")
                            }
                            DeclKind::Function { index } => Ok(out
                                .alloc(Expr::new(ast.location, ExprKind::HostFunctionRef(index)))
                                .with_type(member_decl.typ.clone())),
                            DeclKind::NativeType { .. } => todo!(),
                            DeclKind::TypeAlias => todo!(),
                        }
                    }

                    _ => Err(CompilationError::NoMembers(
                        head[0].location,
                        head_name.clone(),
                    )),
                },
                None => Err(CompilationError::UnknownSymbol(
                    head[0].location,
                    head_name.to_string(),
                )),
            }
        }

        NodeKind::BinaryExpr { op, lhs, rhs } => {
            let lhs_expr = build_exprs(lhs, host, scope, function, inference, out)?;
            let rhs_expr = build_exprs(rhs, host, scope, function, inference, out)?;
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
            let lhs_expr = build_exprs(lhs, host, scope, function, inference, out)?;
            let rhs_expr = build_exprs(rhs, host, scope, function, inference, out)?;

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
            let lhs_expr = build_exprs(lhs, host, scope, function, inference, out)?;
            let rhs_expr = build_exprs(rhs, host, scope, function, inference, out)?;
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
            let arg_expr = build_exprs(arg, host, scope, function, inference, out)?;
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
            let base_expr = build_exprs(base, host, scope, function, inference, out)?;
            match base_expr.typ.clone() {
                ExprType::Entity => {
                    let nt = host.get_host_type::<Entity>();
                    let Some(entity_type) = nt else {
                        panic!("'Entity' type has not been registered");
                    };
                    if let Some(field) = entity_type.decls.get(fname) {
                        match &field.kind {
                            DeclKind::Global { is_const: _, index } => {
                                // assign_types(arg_expr, &infer)?;
                                Ok(out
                                    .alloc(Expr::new(
                                        ast.location,
                                        ExprKind::NativeProp(base_expr, *index),
                                    ))
                                    .with_type(field.typ.clone()))
                            }
                            DeclKind::Function { index: _ } => todo!(),
                            _ => panic!("Invalid entity member"),
                        }
                    } else {
                        Err(CompilationError::UnknownField(
                            ast.location,
                            SmolStr::new_static("Entity"),
                            fname.clone(),
                        ))
                    }
                }

                ExprType::String => {
                    let string_type = host
                        .get_host_type::<String>()
                        .expect("String host type not registered");
                    if let Some(field) = string_type.decls.get(fname) {
                        match &field.kind {
                            DeclKind::Global { is_const: _, index } => {
                                // assign_types(arg_expr, &infer)?;
                                Ok(out
                                    .alloc(Expr::new(
                                        ast.location,
                                        ExprKind::NativeProp(base_expr, *index),
                                    ))
                                    .with_type(field.typ.clone()))
                            }
                            DeclKind::Function { index } => Ok(out
                                .alloc(Expr::new(
                                    ast.location,
                                    ExprKind::HostMethodRef(base_expr, *index),
                                ))
                                .with_type(field.typ.clone())),
                            _ => panic!("Invalid entity member"),
                        }
                    } else {
                        Err(CompilationError::UnknownField(
                            ast.location,
                            SmolStr::new_static("String"),
                            fname.clone(),
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
                ExprType::Reflected(type_info) => match type_info {
                    TypeInfo::Struct(struct_info) => {
                        if let Some(host_type) = host.get_host_type_by_id(type_info.type_id())
                            && let Some(field_decl) = host_type.get(fname)
                        {
                            match field_decl.kind {
                                DeclKind::Global { .. } => todo!(),
                                DeclKind::Local { .. } => todo!(),
                                DeclKind::Param { .. } => todo!(),
                                DeclKind::Function { index } => Ok(out
                                    .alloc(Expr::new(
                                        ast.location,
                                        ExprKind::HostMethodRef(base_expr, index),
                                    ))
                                    .with_type(field_decl.typ.clone())),
                                DeclKind::NativeType { .. } => todo!(),
                                DeclKind::TypeAlias => todo!(),
                            }
                        } else if let Some(field) = struct_info.field(fname)
                            && let Some(index) = struct_info.index_of(fname)
                        {
                            Ok(out
                                .alloc(Expr::new(ast.location, ExprKind::Field(base_expr, index)))
                                .with_type(ExprType::from_type_info(field.type_info().unwrap())))
                        } else {
                            Err(CompilationError::UnknownField(
                                ast.location,
                                SmolStr::new_static(type_info.type_path()),
                                fname.clone(),
                            ))
                        }
                    }
                    TypeInfo::TupleStruct(_tuple_struct_info) => todo!(),
                    TypeInfo::Tuple(_tuple_info) => todo!(),
                    TypeInfo::List(_list_info) => todo!(),
                    TypeInfo::Array(_array_info) => todo!(),
                    TypeInfo::Map(_map_info) => todo!(),
                    TypeInfo::Set(_set_info) => todo!(),
                    TypeInfo::Enum(_enum_info) => todo!(),
                    TypeInfo::Opaque(_opaque_info) => todo!(),
                },

                _ => Err(CompilationError::NoFields(
                    ast.location,
                    base_expr.typ.clone(),
                )),
            }
        }

        NodeKind::FieldIndex(base, _index) => {
            let base_expr = build_exprs(base, host, scope, function, inference, out)?;
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
        NodeKind::Decl(_decl) => {
            panic!("Local declarations should only appear at block level.");
        }

        NodeKind::Block(stmts, result) => {
            build_block(ast, host, scope, function, inference, out, stmts, result)
        }

        NodeKind::Cast { arg, typ } => {
            let mut infer = TypeInference::default();
            let to_typ = resolve_types(scope, typ)?;
            let arg_expr = build_exprs(arg, host, scope, function, &mut infer, out)?;
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
            let func_expr = build_exprs(func, host, scope, function, inference, out)?;
            let mut arg_exprs = Vec::<&mut Expr>::new();
            for arg in args.iter() {
                let arg_expr = build_exprs(arg, host, scope, function, inference, out)?;
                arg_exprs.push(arg_expr);
            }

            // let ret_type = inference.fresh_typevar();
            let fty = match func_expr.typ.clone() {
                ExprType::Function(fty) => fty.clone(),
                _ => {
                    // eprintln!("{}", func_expr.typ);
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
            condition,
            then_block,
            else_block,
        } => {
            let condition_expr = build_exprs(condition, host, scope, function, inference, out)?;
            inference.add_constraint(
                ExprType::Boolean,
                condition_expr.typ.clone(),
                condition.location,
            );

            let then_branch = build_exprs(then_block, host, scope, function, inference, out)?;

            if let Some(else_block) = else_block {
                let else_branch = build_exprs(else_block, host, scope, function, inference, out)?;
                let ty = inference.fresh_typevar();
                // Both sides must be the same, which is also the result type.
                inference.add_constraint(ty.clone(), then_branch.typ.clone(), then_block.location);
                inference.add_constraint(ty.clone(), else_branch.typ.clone(), else_block.location);
                Ok(out
                    .alloc(Expr::new(
                        ast.location,
                        ExprKind::If {
                            condition: condition_expr,
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
                            condition: condition_expr,
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
    scope: &decl::Scope,
    function: &mut FunctionBody,
    inference: &mut TypeInference,
    out: &'e bumpalo::Bump,
    stmts: &[&ASTNode<'_>],
    result: &Option<&ASTNode<'_>>,
) -> Result<&'e mut Expr<'e>, CompilationError> {
    let mut stmt_exprs = Vec::<&mut Expr>::new();

    let mut local_decls = DeclTable::default();
    for stmt in stmts {
        let stmt_expr = build_stmt(
            stmt,
            host,
            scope,
            &mut local_decls,
            function,
            inference,
            out,
        )?;
        stmt_exprs.push(stmt_expr);
    }

    let result_expr = match result {
        Some(result) => Some(build_stmt(
            result,
            host,
            scope,
            &mut local_decls,
            function,
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

fn build_stmt<'a, 'e>(
    ast: &'a ASTNode<'a>,
    host: &HostState,
    parent_scope: &decl::Scope,
    block_scope: &mut decl::DeclTable,
    function: &mut FunctionBody,
    inference: &mut TypeInference,
    out: &'e bumpalo::Bump,
) -> Result<&'e mut Expr<'e>, CompilationError> {
    match &ast.kind {
        NodeKind::Decl(decl) => match decl {
            ASTDecl::Let {
                name,
                typ,
                value,
                is_const,
                visibility,
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
                        function,
                        inference,
                        out,
                    )?),
                    None => None,
                };
                let typ = match (typ, &value_expr) {
                    (Some(typ), Some(value)) => {
                        inference.add_constraint(typ.clone(), value.typ.clone(), value.location);
                        typ
                    }
                    (Some(typ), None) => typ,
                    (None, Some(value)) => value.typ.clone(),
                    (None, None) => {
                        return Err(CompilationError::MissingType(ast.location, name.clone()));
                    }
                };

                let local_index = function.locals.len();
                function.locals.push(typ.clone());

                block_scope.insert(
                    name.clone(),
                    Decl {
                        location: ast.location,
                        visibility: *visibility,
                        typ: typ.clone(),
                        kind: DeclKind::Local {
                            is_const: *is_const,
                            index: local_index,
                        },
                    },
                );

                if let Some(init) = value_expr {
                    Ok(out.alloc(Expr::new(
                        ast.location,
                        ExprKind::Assign {
                            lhs: out
                                .alloc(Expr::new(ast.location, ExprKind::LocalRef(local_index)))
                                .with_type(typ),
                            rhs: init,
                        },
                    )))
                } else {
                    Ok(out.alloc(Expr::new(ast.location, ExprKind::Empty)))
                }
            }
            _ => todo!("Decl: {:?}", decl),
        },

        _ => {
            let block_scope = Scope {
                parent: Some(parent_scope),
                scope_type: ScopeType::Local,
                decls: block_scope,
            };
            build_exprs(ast, host, &block_scope, function, inference, out)
        }
    }
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
            scope_type: ScopeType::Host,
        };
        let mut function = FunctionBody::default();
        let mut inference = TypeInference::default();

        let result = build_exprs(&ast, &host, &scope, &mut function, &mut inference, &arena);
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
            scope_type: ScopeType::Host,
        };
        let mut function = FunctionBody::default();
        let mut inference = TypeInference::default();

        let result = build_exprs(&ast, &host, &scope, &mut function, &mut inference, &arena);
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
            scope_type: ScopeType::Host,
        };
        let mut function = FunctionBody::default();
        let mut inference = TypeInference::default();

        let result = build_exprs(&ast, &host, &scope, &mut function, &mut inference, &arena);
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
            scope_type: ScopeType::Host,
        };
        let mut function = FunctionBody::default();
        let mut inference = TypeInference::default();

        let result = build_exprs(&ast, &host, &scope, &mut function, &mut inference, &arena);
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
            scope_type: ScopeType::Host,
        };
        let mut function = FunctionBody::default();
        let mut inference = TypeInference::default();

        let result = build_exprs(&ast, &host, &scope, &mut function, &mut inference, &arena);
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
            scope_type: ScopeType::Host,
        };
        let mut function = FunctionBody::default();
        let mut inference = TypeInference::default();

        let result = build_exprs(&ast, &host, &scope, &mut function, &mut inference, &arena);
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
            scope_type: ScopeType::Host,
        };
        let mut function = FunctionBody::default();
        let mut inference = TypeInference::default();

        let result = build_exprs(&ast, &host, &scope, &mut function, &mut inference, &arena);
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
            scope_type: ScopeType::Host,
        };
        let mut function = FunctionBody::default();
        let mut inference = TypeInference::default();

        let result = build_exprs(&ast, &host, &scope, &mut function, &mut inference, &arena);
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
            scope_type: ScopeType::Host,
        };
        let mut function = FunctionBody::default();
        let mut inference = TypeInference::default();

        let result = build_exprs(&ast, &host, &scope, &mut function, &mut inference, &arena);
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
            scope_type: ScopeType::Host,
        };
        let mut function = FunctionBody::default();
        let mut inference = TypeInference::default();

        let result = build_exprs(&ast, &host, &scope, &mut function, &mut inference, &arena);
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
            scope_type: ScopeType::Host,
        };
        let mut function = FunctionBody::default();
        let mut inference = TypeInference::default();

        let result = build_exprs(&ast, &host, &scope, &mut function, &mut inference, &arena);

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
            scope_type: ScopeType::Host,
        };
        let mut function = FunctionBody::default();
        let mut inference = TypeInference::default();

        let result = build_exprs(&ast, &host, &scope, &mut function, &mut inference, &arena);

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
