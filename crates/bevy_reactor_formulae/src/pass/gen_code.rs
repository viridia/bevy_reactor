use core::result;

use bevy::{math::ops::exp, scene::ron::de};

use crate::{
    Module,
    ast::{ASTNode, NodeKind},
    compiler::{CompilationError, CompiledFunction, FunctionBody, ModuleExprs},
    decl::{self, FunctionParam, Scope},
    expr::{Expr, ExprKind},
    expr_type::ExprType,
    instr::{self, InstructionBuilder, OP_CONST_I32, OP_LOAD_LOCAL},
    oper::{BinaryOp, UnaryOp},
};

pub(crate) fn gen_module<'content>(
    module: &mut Module,
    module_exprs: &'content ModuleExprs<'content>,
) -> Result<(), CompilationError> {
    // let mut generator = CodeGenerator::default();
    module
        .functions
        .resize_with(module_exprs.functions.len(), || CompiledFunction {
            code: Vec::new(),
            num_locals: 0,
        });

    for (_, decl) in module.module_decls.iter() {
        match &decl.kind {
            decl::DeclKind::Global {
                is_const: _,
                index: _,
            } => todo!(),
            decl::DeclKind::Local {
                is_const: _,
                index: _,
            } => unreachable!(),
            decl::DeclKind::Function { index, .. } => {
                let mut builder = instr::InstructionBuilder::default();
                let function = &module_exprs.functions[*index];
                if let Some(body) = function.body {
                    gen_expr(module, function, body, &mut builder).unwrap();
                    if body.typ.is_void() {
                        builder.push_op(instr::OP_CONST_VOID);
                    }
                    builder.push_op(instr::OP_RET);
                }
                module.functions[*index].code = builder.inner();
                module.functions[*index].num_locals = function.locals.len();
            }
            _ => {}
        }
    }

    Ok(())
}

fn gen_expr<'cu>(
    module: &'cu Module,
    function: &'cu FunctionBody,
    expr: &'cu Expr,
    out: &mut InstructionBuilder,
) -> Result<(), CompilationError> {
    match expr.kind {
        ExprKind::Empty => todo!(),
        ExprKind::ConstBool(value) => {
            match value {
                true => out.push_op(instr::OP_CONST_TRUE),
                false => out.push_op(instr::OP_CONST_FALSE),
            };
        }
        ExprKind::ConstInteger(value) => {
            match expr.typ {
                ExprType::I32 => {
                    out.push_op(instr::OP_CONST_I32);
                    out.push_immediate(value as i32);
                }
                ExprType::I64 => {
                    out.push_op(instr::OP_CONST_I64);
                    out.push_immediate(value);
                }
                _ => panic!("Invalid integer type: {:?}", expr.typ),
            };
        }
        ExprKind::ConstFloat(value) => {
            match expr.typ {
                ExprType::F32 => {
                    out.push_op(instr::OP_CONST_F32);
                    out.push_immediate(value as f32);
                }
                ExprType::F64 => {
                    out.push_op(instr::OP_CONST_F64);
                    out.push_immediate(value);
                }
                _ => panic!("Invalid float type: {:?}", expr.typ),
            };
        }

        ExprKind::ConstString(ref str) => {
            let str_len = str.len();
            if str_len > u16::MAX as usize {
                panic!("String constants longer than 64k are not supported");
            }

            out.push_op(instr::OP_CONST_STR);
            out.push_immediate(str_len as u16);
            out.push_immediate_string(str);
        }

        // ExprKind::FunctionRef(index) => {
        //     panic!("Cannot codegen function reference: {:?}", index);
        // }
        ExprKind::ParamRef(index) => {
            out.push_op(instr::OP_LOAD_PARAM);
            out.push_immediate::<u32>(index as u32);
        }

        ExprKind::LocalRef(index) => {
            local_get(function, index, out);
        }

        ExprKind::Field(ref base, field_index) => {
            gen_expr(module, function, base, out)?;
            match base.typ {
                ExprType::Reflected(_) => {
                    out.push_op(instr::OP_LOAD_FIELD);
                    out.push_immediate::<u16>(field_index as u16);
                }
                _ => panic!("Invalid type for field access"),
            }

            // let field = match &base.typ {
            //     ExprType::Struct(stype) => &stype.fields[field_index],
            //     _ => panic!("Invalid field access: {:?}", base.typ),
            // };

            // match base.kind {
            //     ExprKind::LocalRef(index) => {
            //         let local = &generator.locals[index];
            //         local_get(local.local_index + field.index, &field.typ, out);
            //     }
            //     _ => todo!(),
            // }
        }

        ExprKind::NativeProp(ref base, field_index) => {
            gen_expr(module, function, base, out)?;
            out.push_op(instr::OP_LOAD_NATIVE_PROP);
            out.push_immediate::<u16>(field_index as u16);
        }

        // ExprKind::Index(ref base, _field_index) => {
        //     gen_expr(unit, generator, base, out)?;
        //     todo!();
        // }
        ExprKind::GlobalRef(index) => {
            out.push_op(instr::OP_LOAD_GLOBAL);
            out.push_immediate::<u16>(index as u16);
        }

        // ExprKind::LocalDecl(index, ref init) => {
        //     if let Some(init) = init {
        //         gen_expr(unit, generator, init, out)?;
        //         let local = &generator.locals[index];
        //         local_set(local.local_index, &local.typ, out);
        //     }
        // }
        ExprKind::BinaryExpr {
            op,
            ref lhs,
            ref rhs,
        } => {
            gen_expr(module, function, lhs, out)?;
            gen_expr(module, function, rhs, out)?;
            gen_binop(&expr.typ, op, &lhs.typ, &rhs.typ, out);
        }

        ExprKind::Assign { ref lhs, ref rhs } => {
            // gen_expr(unit, generator, rhs, out)?;
            gen_assign(module, function, lhs, rhs, None, out)?;
        }

        // ExprKind::AssignOp {
        //     op,
        //     ref lhs,
        //     ref rhs,
        // } => {
        //     // gen_expr(unit, generator, rhs, out)?;
        //     gen_assign(unit, generator, lhs, rhs, Some(op), out)?;
        // }
        ExprKind::UnaryExpr { op, ref arg } => {
            gen_expr(module, function, arg, out)?;
            match op {
                UnaryOp::Not => todo!(),
                UnaryOp::Neg => todo!(),
                UnaryOp::BitNot => todo!(),
            }
        }

        // ExprKind::Cast(ref arg) => {
        //     gen_expr(unit, generator, arg, out)?;
        //     // Convert from arg.typ to expr.typ.
        //     match (&expr.typ, &arg.typ) {
        //         (ExprType::Boolean, ExprType::I32) => {
        //             out.instruction(&Instruction::I32Eqz);
        //         }
        //         (ExprType::Boolean, ExprType::I64) => {
        //             out.instruction(&Instruction::I64Eqz);
        //             out.instruction(&Instruction::I32WrapI64);
        //         }
        //         (ExprType::I32, ExprType::Boolean) => {
        //             // Same type, do nothing.
        //         }
        //         (ExprType::I32, ExprType::I64) => {
        //             out.instruction(&Instruction::I32WrapI64);
        //         }
        //         (ExprType::I32, ExprType::F32) => {
        //             out.instruction(&Instruction::I32TruncF32S);
        //         }
        //         (ExprType::I32, ExprType::F64) => {
        //             out.instruction(&Instruction::I32TruncF64S);
        //         }
        //         (ExprType::I64, ExprType::Boolean) => {
        //             out.instruction(&Instruction::I64ExtendI32U);
        //         }
        //         (ExprType::I64, ExprType::I32) => {
        //             out.instruction(&Instruction::I64Extend32S);
        //         }
        //         (ExprType::I64, ExprType::F32) => {
        //             out.instruction(&Instruction::I64TruncF32S);
        //         }
        //         (ExprType::I64, ExprType::F64) => {
        //             out.instruction(&Instruction::I64TruncF64S);
        //         }
        //         (ExprType::F32, ExprType::I32) => {
        //             out.instruction(&Instruction::F32ConvertI32S);
        //         }
        //         (ExprType::F32, ExprType::I64) => {
        //             out.instruction(&Instruction::F32ConvertI64S);
        //         }
        //         (ExprType::F32, ExprType::F64) => {
        //             out.instruction(&Instruction::F32DemoteF64);
        //         }
        //         (ExprType::F64, ExprType::I32) => {
        //             out.instruction(&Instruction::F64ConvertI32S);
        //         }
        //         (ExprType::F64, ExprType::I64) => {
        //             out.instruction(&Instruction::F64ConvertI64S);
        //         }
        //         (ExprType::F64, ExprType::F32) => {
        //             out.instruction(&Instruction::F64PromoteF32);
        //         }
        //         _ => panic!("Invalid cast: {:?}", expr),
        //     }
        // }
        ExprKind::Call(ref func, ref args) => {
            if let ExprKind::FunctionRef(scope_type, index) = func.kind {
                for arg in args {
                    gen_expr(module, function, arg, out)?;
                }
                // println!("Call function: {}", index);
                match scope_type {
                    decl::ScopeType::Host => {
                        out.push_op(instr::OP_CALL_HOST_METHOD);
                        out.push_immediate::<u32>(index as u32);
                    }
                    decl::ScopeType::Module => {
                        out.push_op(instr::OP_CALL);
                        out.push_immediate::<u32>(index as u32);
                    }
                    decl::ScopeType::Object | decl::ScopeType::String => {
                        panic!("Method call without base expression");
                    }
                    decl::ScopeType::Import => todo!(),
                    decl::ScopeType::Param => todo!(),
                    decl::ScopeType::Local => todo!(),
                }
                let num_args = args.len();
                if num_args > u16::MAX as usize {
                    panic!("Too many function arguments: {num_args}");
                }
                out.push_immediate::<u16>(num_args as u16);
            } else if let ExprKind::MethodRef(_scope_type, base, index) = &func.kind {
                gen_expr(module, function, base, out)?;
                for arg in args {
                    gen_expr(module, function, arg, out)?;
                }
                out.push_op(instr::OP_CALL_OBJECT_METHOD);
                out.push_immediate::<u16>(*index as u16);
                let num_args = args.len() + 1;
                if num_args > u16::MAX as usize {
                    panic!("Too many function arguments: {num_args}");
                }
                out.push_immediate::<u16>(num_args as u16);
            } else {
                panic!("Invalid function reference: {func:?}");
            }
        }

        ExprKind::Block(ref vec, ref expr) => {
            for stmt in vec {
                gen_expr(module, function, stmt, out)?;
                match stmt.typ {
                    ExprType::Void | ExprType::None => {}
                    _ => {
                        out.push_op(instr::OP_DROP);
                    }
                }
            }

            if let Some(expr) = expr {
                gen_expr(module, function, expr, out)?;
            }
        }

        ExprKind::If {
            ref condition,
            ref then_branch,
            ref else_branch,
        } => {
            gen_expr(module, function, condition, out)?;
            out.push_op(instr::OP_BRANCH_IF_FALSE);
            let false_branch = out.reserve_immediate::<i32>();
            gen_expr(module, function, then_branch, out)?;
            if let Some(else_branch) = else_branch {
                // Branch at end of then block
                out.push_op(instr::OP_BRANCH);
                let end_if = out.reserve_immediate::<i32>();

                // Patch first jump to go to this block.
                out.patch_branch_target(false_branch, out.position());
                gen_expr(module, function, else_branch, out)?;

                // Patch second jump
                out.patch_branch_target(end_if, out.position());
            } else {
                out.patch_branch_target(false_branch, out.position());
            }
        }

        _ => todo!("Unimplemented {:?}", expr.kind),
    }

    Ok(())
}

fn local_get(function: &FunctionBody, local_index: usize, out: &mut InstructionBuilder) {
    let local_type = &function.locals[local_index];
    match local_type {
        ExprType::None | ExprType::Void => {}
        ExprType::IUnsized | ExprType::Infer(_) => unreachable!("Unknown type for local"),
        ExprType::Boolean
        | ExprType::I32
        | ExprType::I64
        | ExprType::F32
        | ExprType::F64
        | ExprType::String => {
            out.push_op(instr::OP_LOAD_LOCAL);
            out.push_immediate::<u16>((local_index + function.num_params) as u16);
        }
        // ExprType::Array(_element) => {
        //     todo!();
        // }
        // ExprType::Struct(stype) => {
        //     if stype.is_record {
        //         out.instruction(&Instruction::LocalGet(local_index as u32));
        //     } else {
        //         for field in &stype.fields {
        //             local_get(local_index, &field.typ, out);
        //             local_index += field.typ.value_count();
        //         }
        //     }
        // }
        // ExprType::TupleStruct(stype) => {
        //     if stype.is_record {
        //         out.instruction(&Instruction::LocalGet(local_index as u32));
        //     } else {
        //         for typ in &stype.fields {
        //             local_get(local_index, typ, out);
        //             local_index += typ.value_count();
        //         }
        //     }
        // }
        // ExprType::Tuple(members) => {
        //     for member in members.iter() {
        //         local_get(local_index, member, out);
        //         local_index += member.value_count();
        //     }
        // }
        ExprType::Function(_ftype) => {
            todo!();
        }
        _ => todo!(),
    }
}

fn local_set(function: &FunctionBody, local_index: usize, out: &mut InstructionBuilder) {
    let local_type = &function.locals[local_index];
    match local_type {
        ExprType::None | ExprType::Void => {}
        ExprType::IUnsized | ExprType::Infer(_) => unreachable!(),
        ExprType::Boolean
        | ExprType::I32
        | ExprType::I64
        | ExprType::F32
        | ExprType::F64
        | ExprType::String => {
            out.push_op(instr::OP_STORE_LOCAL);
            out.push_immediate::<u16>((local_index + function.num_params) as u16);
        }
        // ExprType::Array(_element) => {
        //     todo!();
        // }
        // ExprType::Struct(stype) => {
        //     if stype.is_record {
        //         out.instruction(&Instruction::LocalSet(local_index as u32));
        //     } else {
        //         for field in stype.fields.iter().rev() {
        //             local_set(local_index, &field.typ, out);
        //             local_index += field.typ.value_count();
        //         }
        //     }
        // }
        // ExprType::TupleStruct(stype) => {
        //     if stype.is_record {
        //         out.instruction(&Instruction::LocalSet(local_index as u32));
        //     } else {
        //         for typ in stype.fields.iter().rev() {
        //             local_set(local_index, typ, out);
        //             local_index += typ.value_count();
        //         }
        //     }
        // }
        // ExprType::Tuple(members) => {
        //     for member in members.iter().rev() {
        //         local_set(local_index, member, out);
        //         local_index += member.value_count();
        //     }
        // }
        ExprType::Function(_ftype) => {
            todo!();
        }
        _ => todo!(),
    }
}

fn gen_assign<'cu>(
    module: &'cu Module,
    function: &'cu FunctionBody,
    lhs: &Expr,
    rhs: &Expr,
    op: Option<BinaryOp>,
    out: &mut InstructionBuilder,
) -> Result<(), CompilationError> {
    match lhs.kind {
        ExprKind::ParamRef(_)
        | ExprKind::LocalRef(_)
        | ExprKind::GlobalRef(_)
        | ExprKind::Field(_, _)
        | ExprKind::Index(_, _) => {
            let (lvalue, lvalue_type) = expr_to_lvalue(function, lhs, out)?;
            match lvalue {
                LValue::Local(local_index) => {
                    if op.is_some() {
                        local_get(function, local_index, out);
                    }
                    gen_expr(module, function, rhs, out)?;
                    if let Some(bop) = op {
                        gen_binop(&lvalue_type, bop, &lhs.typ, &rhs.typ, out);
                        // gen_binop(&expr.typ, op, &lhs.typ, &rhs.typ, out);
                    }
                    local_set(function, local_index, out);
                }
                _ => todo!(),
            }
        }
        _ => return Err(CompilationError::InvalidAssignmentTarget(lhs.location)),
    }

    Ok(())
}

enum LValue {
    Local(usize),
    #[allow(unused)]
    Global(usize),
    // Memory?
    // Reference?
}

fn expr_to_lvalue(
    function: &FunctionBody,
    expr: &Expr,
    _out: &mut InstructionBuilder,
) -> Result<(LValue, ExprType), CompilationError> {
    match expr.kind {
        ExprKind::ParamRef(index) => {
            // let param = &generator.params[index];
            Ok((LValue::Local(index), expr.typ.clone()))
        }
        ExprKind::LocalRef(index) => {
            // let local = &generator.locals[index];
            Ok((LValue::Local(index), expr.typ.clone()))
        }
        ExprKind::GlobalRef(_index) => todo!(),
        ExprKind::Field(ref _base, _field_index) => todo!(),
        // ExprKind::Field(ref base, field_index) => {
        //     let (base_lvalue, base_type) = expr_to_lvalue(unit, generator, base, out)?;
        //     match base_type {
        //         ExprType::Struct(stype) => {
        //             if stype.is_record {
        //                 gen_expr(unit, generator, base, out)?;
        //                 todo!();
        //             } else {
        //                 let field = &stype.fields[field_index];
        //                 match base_lvalue {
        //                     LValue::Local(local_index) => {
        //                         Ok((LValue::Local(local_index + field.index), field.typ.clone()))
        //                     }
        //                     _ => todo!(),
        //                 }
        //             }
        //         }
        //         _ => panic!("Invalid field access: {:?}", base_type),
        //     }
        // }
        ExprKind::Index(ref _expr, _) => todo!(),
        _ => panic!("Invalid lvalue: {expr:?}"),
    }
}

fn gen_binop(
    typ: &ExprType,
    op: BinaryOp,
    lhs_type: &ExprType,
    rhs_type: &ExprType,
    out: &mut InstructionBuilder,
) {
    match op {
        BinaryOp::Add => {
            match typ {
                ExprType::I32 => out.push_op(instr::OP_ADD_I32),
                ExprType::I64 => out.push_op(instr::OP_ADD_I64),
                ExprType::F32 => out.push_op(instr::OP_ADD_F32),
                ExprType::F64 => out.push_op(instr::OP_ADD_F64),
                _ => panic!("Invalid type for binary addition: {typ:?}"),
            };
        }
        BinaryOp::Sub => {
            match typ {
                ExprType::I32 => out.push_op(instr::OP_SUB_I32),
                ExprType::I64 => out.push_op(instr::OP_SUB_I64),
                ExprType::F32 => out.push_op(instr::OP_SUB_F32),
                ExprType::F64 => out.push_op(instr::OP_SUB_F64),
                _ => panic!("Invalid type for binary subtraction: {typ:?}"),
            };
        }
        BinaryOp::Mul => {
            match typ {
                ExprType::I32 => out.push_op(instr::OP_MUL_I32),
                ExprType::I64 => out.push_op(instr::OP_MUL_I64),
                ExprType::F32 => out.push_op(instr::OP_MUL_F32),
                ExprType::F64 => out.push_op(instr::OP_MUL_F64),
                _ => panic!("Invalid type for binary multiplication: {typ:?}"),
            };
        }
        BinaryOp::Div => {
            match typ {
                ExprType::I32 => out.push_op(instr::OP_DIV_I32),
                ExprType::I64 => out.push_op(instr::OP_DIV_I64),
                ExprType::F32 => out.push_op(instr::OP_DIV_F32),
                ExprType::F64 => out.push_op(instr::OP_DIV_F64),
                _ => panic!("Invalid type for binary division: {typ:?}"),
            };
        }
        BinaryOp::Mod => {
            match typ {
                ExprType::I32 => out.push_op(instr::OP_REM_I32),
                ExprType::I64 => out.push_op(instr::OP_REM_I64),
                _ => panic!("Invalid type for binary modulo: {typ:?}"),
            };
        }
        BinaryOp::LogAnd => {
            // TODO: Short-circuiting.
            match typ {
                ExprType::Boolean => out.push_op(instr::OP_LOGICAL_AND),
                _ => panic!("Invalid type for logical AND: {typ:?}"),
            };
        }
        BinaryOp::LogOr => {
            // TODO: Short-circuiting.
            match typ {
                ExprType::Boolean => out.push_op(instr::OP_LOGICAL_OR),
                _ => panic!("Invalid type for logical OR: {typ:?}"),
            };
        }
        BinaryOp::BitAnd => {
            match typ {
                ExprType::I32 => out.push_op(instr::OP_BIT_AND_I32),
                ExprType::I64 => out.push_op(instr::OP_BIT_AND_I64),
                _ => panic!("Invalid type for bitwise AND: {typ:?}"),
            };
        }
        BinaryOp::BitOr => {
            match typ {
                ExprType::I32 => out.push_op(instr::OP_BIT_OR_I32),
                ExprType::I64 => out.push_op(instr::OP_BIT_OR_I64),
                _ => panic!("Invalid type for bitwise OR: {typ:?}"),
            };
        }
        BinaryOp::BitXor => {
            match typ {
                ExprType::I32 => out.push_op(instr::OP_BIT_XOR_I32),
                ExprType::I64 => out.push_op(instr::OP_BIT_XOR_I64),
                _ => panic!("Invalid type for bitwise XOR: {typ:?}"),
            };
        }
        BinaryOp::Shl => {
            match typ {
                ExprType::I32 => out.push_op(instr::OP_SHL),
                ExprType::I64 => out.push_op(instr::OP_SHL),
                _ => panic!("Invalid type for bitwise shift left: {typ:?}"),
            };
        }
        BinaryOp::Shr => {
            match typ {
                ExprType::I32 => out.push_op(instr::OP_SHR),
                ExprType::I64 => out.push_op(instr::OP_SHR),
                _ => panic!("Invalid type for bitwise shift right: {typ:?}"),
            };
        }
        BinaryOp::Eq => {
            assert_eq!(*lhs_type, *rhs_type);
            match lhs_type {
                ExprType::I32 => out.push_op(instr::OP_EQ_I32),
                ExprType::I64 => out.push_op(instr::OP_EQ_I64),
                ExprType::F32 => out.push_op(instr::OP_EQ_F32),
                ExprType::F64 => out.push_op(instr::OP_EQ_F64),
                _ => panic!("Invalid type for equality comparison: {typ:?}"),
            };
        }
        BinaryOp::Ne => {
            assert_eq!(*lhs_type, *rhs_type);
            match lhs_type {
                ExprType::I32 => out.push_op(instr::OP_NE_I32),
                ExprType::I64 => out.push_op(instr::OP_NE_I64),
                ExprType::F32 => out.push_op(instr::OP_NE_F32),
                ExprType::F64 => out.push_op(instr::OP_NE_F64),
                _ => panic!("Invalid type for inequality comparison: {typ:?}"),
            };
        }
        BinaryOp::Lt => {
            assert_eq!(*lhs_type, *rhs_type);
            match lhs_type {
                ExprType::I32 => out.push_op(instr::OP_LT_I32),
                ExprType::I64 => out.push_op(instr::OP_LT_I64),
                ExprType::F32 => out.push_op(instr::OP_LT_F32),
                ExprType::F64 => out.push_op(instr::OP_LT_F64),
                _ => panic!("Invalid type for less-than comparison: {typ:?}"),
            };
        }
        BinaryOp::Le => {
            assert_eq!(*lhs_type, *rhs_type);
            match lhs_type {
                ExprType::I32 => out.push_op(instr::OP_LE_I32),
                ExprType::I64 => out.push_op(instr::OP_LE_I64),
                ExprType::F32 => out.push_op(instr::OP_LE_F32),
                ExprType::F64 => out.push_op(instr::OP_LE_F64),
                _ => panic!("Invalid type for less-than-or-equal comparison: {typ:?}"),
            };
        }
        BinaryOp::Gt => {
            assert_eq!(*lhs_type, *rhs_type);
            match lhs_type {
                ExprType::I32 => out.push_op(instr::OP_GT_I32),
                ExprType::I64 => out.push_op(instr::OP_GT_I64),
                ExprType::F32 => out.push_op(instr::OP_GT_F32),
                ExprType::F64 => out.push_op(instr::OP_GT_F64),
                _ => panic!("Invalid type for greater-than comparison: {typ:?}"),
            };
        }
        BinaryOp::Ge => {
            assert_eq!(*lhs_type, *rhs_type);
            match lhs_type {
                ExprType::I32 => out.push_op(instr::OP_GE_I32),
                ExprType::I64 => out.push_op(instr::OP_GE_I64),
                ExprType::F32 => out.push_op(instr::OP_GE_F32),
                ExprType::F64 => out.push_op(instr::OP_GE_F64),
                _ => panic!("Invalid type for greater-than-or-equal comparison: {typ:?}"),
            };
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::host::HostState;

    use super::*;

    #[test]
    fn test_gen_expr() {
        let mut builder = instr::InstructionBuilder::default();
        let module = Module::default();
        let function = FunctionBody::default();

        // Create the expression: 2 + 2
        let mut lhs = Expr {
            kind: ExprKind::ConstInteger(2),
            typ: ExprType::I32,
            location: Default::default(),
        };
        let mut rhs = Expr {
            kind: ExprKind::ConstInteger(2),
            typ: ExprType::I32,
            location: Default::default(),
        };
        let expr = Expr {
            kind: ExprKind::BinaryExpr {
                op: BinaryOp::Add,
                lhs: &mut lhs,
                rhs: &mut rhs,
            },
            typ: ExprType::I32,
            location: Default::default(),
        };

        gen_expr(&module, &function, &expr, &mut builder).unwrap();

        let instructions = builder.inner();
        assert_eq!(instructions[0], instr::OP_CONST_I32);
        assert_eq!(instructions[8], instr::OP_CONST_I32);
        assert_eq!(instructions[16], instr::OP_ADD_I32);
    }
}
