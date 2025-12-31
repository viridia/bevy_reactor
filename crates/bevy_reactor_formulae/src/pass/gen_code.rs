use core::result;

use bevy::{math::ops::exp, scene::ron::de};

use crate::{
    Module,
    ast::{ASTNode, NodeKind},
    bblock::{BasicBlock, BlockTerminator, StackOp, StackOpType},
    compiler::{CompilationError, CompiledFunction, ModuleExprs},
    decl::{self, FunctionParam, Scope},
    expr::{Expr, ExprKind, FunctionBody},
    expr_type::ExprType,
    instr::{self, InstructionBuilder},
    oper::{BinaryOp, UnaryOp},
};

#[derive(Default, Debug)]
struct BlockBuilder {
    blocks: Vec<BasicBlock>,
    current: usize,
}

impl BlockBuilder {
    fn create_initial_block(&mut self) {
        self.current = self.new_block();
    }

    fn new_block(&mut self) -> usize {
        let id = self.blocks.len();
        self.blocks.push(BasicBlock::default());
        id
    }

    fn add_op(&mut self, op: StackOp) {
        let block = self.blocks.get_mut(self.current).unwrap();
        block.ops.push(op);
    }

    fn set_block_term(&mut self, term: BlockTerminator) {
        let block = self.blocks.get_mut(self.current).unwrap();
        block.term = term;
    }
}

pub(crate) fn gen_code<'content>(
    module: &mut Module,
    module_exprs: &mut ModuleExprs<'content>,
) -> Result<(), CompilationError> {
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
                let function = &mut module_exprs.functions[*index];
                let mut builder = BlockBuilder::default();
                builder.create_initial_block();
                if let Some(body) = function.body {
                    visit_expr(module, function, body, &mut builder).unwrap();
                    if body.typ.is_void() {
                        builder.add_op(StackOp::ConstVoid);
                    }
                    builder.set_block_term(BlockTerminator::Return);
                }

                struct BranchPatch {
                    from_offset: usize,
                    to_block: usize,
                }

                let mut patches: Vec<BranchPatch> = Vec::with_capacity(builder.blocks.len() * 2);
                let mut ibuilder = InstructionBuilder::default();
                for (block_id, block) in builder.blocks.iter_mut().enumerate() {
                    block.start_offset = ibuilder.position();
                    for op in block.ops.iter() {
                        visit_stack_op(op, &mut ibuilder)?;
                    }

                    match block.term {
                        BlockTerminator::Invalid => panic!("Basic block with no terminator"),
                        BlockTerminator::Return => {
                            ibuilder.push_op(instr::OP_RET);
                        }
                        BlockTerminator::Jump { target } => {
                            if target != block_id + 1 {
                                ibuilder.push_op(instr::OP_BRANCH);
                                let patch_offset = ibuilder.reserve_immediate::<i32>();
                                patches.push(BranchPatch {
                                    from_offset: patch_offset,
                                    to_block: target,
                                });
                            }
                        }
                        BlockTerminator::Branch {
                            then_target,
                            else_target,
                        } => {
                            if then_target == block_id + 1 {
                                // Fall through to then-block if true
                                ibuilder.push_op(instr::OP_BRANCH_IF_FALSE);
                                let patch_offset = ibuilder.reserve_immediate::<i32>();
                                patches.push(BranchPatch {
                                    from_offset: patch_offset,
                                    to_block: else_target,
                                });
                            } else if else_target == block_id + 1 {
                                // Fall through to else block if false.
                                ibuilder.push_op(instr::OP_BRANCH_IF_TRUE);
                                let patch_offset = ibuilder.reserve_immediate::<i32>();
                                patches.push(BranchPatch {
                                    from_offset: patch_offset,
                                    to_block: then_target,
                                });
                            } else {
                                ibuilder.push_op(instr::OP_BRANCH_IF_TRUE);
                                let patch_offset = ibuilder.reserve_immediate::<i32>();
                                patches.push(BranchPatch {
                                    from_offset: patch_offset,
                                    to_block: then_target,
                                });

                                // Only branch to else block if we wouldn't fall through.
                                ibuilder.push_op(instr::OP_BRANCH);
                                let patch_offset = ibuilder.reserve_immediate::<i32>();
                                patches.push(BranchPatch {
                                    from_offset: patch_offset,
                                    to_block: else_target,
                                });
                            }
                        }
                    }
                }

                for patch in patches.iter() {
                    let target_block = &builder.blocks[patch.to_block];
                    ibuilder.patch_branch_target(patch.from_offset, target_block.start_offset);
                }

                module.functions[*index].code = ibuilder.inner();
                module.functions[*index].num_locals = function.locals.len();
            }
            _ => {}
        }
    }

    Ok(())
}

fn visit_expr<'cu>(
    module: &'cu Module,
    function: &'cu FunctionBody,
    expr: &'cu Expr,
    out: &mut BlockBuilder,
) -> Result<(), CompilationError> {
    match expr.kind {
        ExprKind::Empty => {}
        ExprKind::ConstBool(b) => {
            out.add_op(StackOp::ConstBool(b));
        }
        ExprKind::ConstInteger(n) => {
            match expr.typ {
                ExprType::I32 => {
                    out.add_op(StackOp::ConstI32(n as i32));
                }
                ExprType::I64 => {
                    out.add_op(StackOp::ConstI64(n));
                }
                _ => panic!("Invalid integer type: {:?}", expr.typ),
            };
        }
        ExprKind::ConstFloat(n) => {
            match expr.typ {
                ExprType::F32 => {
                    out.add_op(StackOp::ConstF32(n as f32));
                }
                ExprType::F64 => {
                    out.add_op(StackOp::ConstF64(n));
                }
                _ => panic!("Invalid float type: {:?}", expr.typ),
            };
        }
        ExprKind::ConstString(ref s) => {
            out.add_op(StackOp::ConstStr(s.clone()));
        }
        ExprKind::ParamRef(index) => {
            out.add_op(StackOp::LoadParam(index as u16));
        }
        ExprKind::LocalRef(index) => {
            out.add_op(StackOp::LoadLocal(index as u16));
        }
        ExprKind::Field(ref base, field_index) => {
            visit_expr(module, function, base, out)?;
            match base.typ {
                ExprType::Reflected(_) => {
                    out.add_op(StackOp::LoadField(field_index as u16));
                }
                _ => panic!("Invalid type for field access"),
            }
        }
        ExprKind::NativeProp(ref base, field_index) => {
            visit_expr(module, function, base, out)?;
            out.add_op(StackOp::LoadNativeProp(field_index as u16));
        }

        // ExprKind::Index(ref base, _field_index) => {
        //     gen_expr(unit, generator, base, out)?;
        //     todo!();
        // }
        ExprKind::GlobalRef(index) => {
            out.add_op(StackOp::LoadGlobal(index as u16));
        }
        ExprKind::BinaryExpr {
            op,
            ref lhs,
            ref rhs,
        } => {
            visit_expr(module, function, lhs, out)?;
            visit_expr(module, function, rhs, out)?;
            match op {
                BinaryOp::Add
                | BinaryOp::Sub
                | BinaryOp::Mul
                | BinaryOp::Div
                | BinaryOp::Mod
                | BinaryOp::LogAnd
                | BinaryOp::LogOr
                | BinaryOp::BitAnd
                | BinaryOp::BitOr
                | BinaryOp::BitXor => {
                    out.add_op(StackOp::BinaryOp(
                        op,
                        match expr.typ {
                            ExprType::I32 => StackOpType::I32,
                            ExprType::I64 => StackOpType::I64,
                            ExprType::F32 => StackOpType::F32,
                            ExprType::F64 => StackOpType::F64,
                            _ => panic!("Invalid type for binary addition: {:?}", expr.typ),
                        },
                    ));
                }
                BinaryOp::Shl => todo!(),
                BinaryOp::Shr => todo!(),
                BinaryOp::Eq
                | BinaryOp::Ne
                | BinaryOp::Lt
                | BinaryOp::Le
                | BinaryOp::Gt
                | BinaryOp::Ge => {
                    out.add_op(StackOp::BinaryOp(
                        op,
                        match lhs.typ {
                            ExprType::I32 => StackOpType::I32,
                            ExprType::I64 => StackOpType::I64,
                            ExprType::F32 => StackOpType::F32,
                            ExprType::F64 => StackOpType::F64,
                            _ => panic!("Invalid type for binary addition: {:?}", lhs.typ),
                        },
                    ));
                }
            }
        }
        ExprKind::Assign { ref lhs, ref rhs } => {
            // gen_expr(unit, generator, rhs, out)?;
            visit_assign(module, function, lhs, rhs, None, out)?;
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
            visit_expr(module, function, arg, out)?;
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
            if let ExprKind::HostFunctionRef(index) = func.kind {
                for arg in args {
                    visit_expr(module, function, arg, out)?;
                }
                let num_args = args.len();
                if num_args > u16::MAX as usize {
                    panic!("Too many function arguments: {num_args}");
                }
                out.add_op(StackOp::CallHostFunction {
                    method: index as u16,
                    num_args: num_args as u16,
                });
            } else if let ExprKind::ScriptFunctionRef(index) = func.kind {
                for arg in args {
                    visit_expr(module, function, arg, out)?;
                }
                let num_args = args.len();
                if num_args > u16::MAX as usize {
                    panic!("Too many function arguments: {num_args}");
                }
                out.add_op(StackOp::CallScriptFunction {
                    method: index as u16,
                    num_args: num_args as u16,
                });
            } else if let ExprKind::HostMethodRef(base, index) = &func.kind {
                visit_expr(module, function, base, out)?;
                for arg in args {
                    visit_expr(module, function, arg, out)?;
                }
                let num_args = args.len() + 1;
                if num_args > u16::MAX as usize {
                    panic!("Too many function arguments: {num_args}");
                }
                out.add_op(StackOp::CallHostMethod {
                    method: *index as u16,
                    num_args: num_args as u16,
                });
            } else if let ExprKind::ScriptMethodRef(_base, _index) = &func.kind {
                todo!();
            } else {
                panic!("Invalid function reference: {func:?}");
            }
        }

        ExprKind::Block(ref vec, ref expr) => {
            for stmt in vec {
                visit_expr(module, function, stmt, out)?;
                match stmt.typ {
                    ExprType::Void | ExprType::None => {}
                    _ => {
                        out.add_op(StackOp::Drop(1));
                    }
                }
            }

            if let Some(expr) = expr {
                visit_expr(module, function, expr, out)?;
            }
        }

        ExprKind::If {
            ref condition,
            ref then_branch,
            ref else_branch,
        } => {
            visit_expr(module, function, condition, out)?;
            let then_block = out.new_block();

            if let Some(else_branch) = else_branch {
                let else_block = out.new_block();
                let next_block = out.new_block();
                out.set_block_term(BlockTerminator::Branch {
                    then_target: then_block,
                    else_target: else_block,
                });
                out.current = then_block;
                visit_expr(module, function, then_branch, out)?;
                out.set_block_term(BlockTerminator::Jump { target: next_block });

                out.current = else_block;
                visit_expr(module, function, else_branch, out)?;
                out.set_block_term(BlockTerminator::Jump { target: next_block });

                out.current = next_block;
            } else {
                let next_block = out.new_block();
                out.set_block_term(BlockTerminator::Branch {
                    then_target: then_block,
                    else_target: next_block,
                });
                out.current = then_block;
                visit_expr(module, function, then_branch, out)?;
                out.set_block_term(BlockTerminator::Jump { target: next_block });

                out.current = next_block;
            }
        }

        _ => todo!("Unimplemented {:?}", expr.kind),
    }

    Ok(())
}

fn local_get(function: &FunctionBody, local_index: usize, out: &mut BlockBuilder) {
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
            out.add_op(StackOp::LoadLocal(
                (local_index + function.num_params) as u16,
            ));
            // out.push_op(instr::OP_LOAD_LOCAL);
            // out.push_immediate::<u16>((local_index + function.num_params) as u16);
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

fn local_set(function: &FunctionBody, local_index: usize, out: &mut BlockBuilder) {
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
            out.add_op(StackOp::StoreLocal(
                (local_index + function.num_params) as u16,
            ));
            // out.push_op(instr::OP_STORE_LOCAL);
            // out.push_immediate::<u16>((local_index + function.num_params) as u16);
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

fn visit_assign<'cu>(
    module: &'cu Module,
    function: &'cu FunctionBody,
    lhs: &Expr,
    rhs: &Expr,
    op: Option<BinaryOp>,
    out: &mut BlockBuilder,
) -> Result<(), CompilationError> {
    match lhs.kind {
        ExprKind::ParamRef(_)
        | ExprKind::LocalRef(_)
        | ExprKind::GlobalRef(_)
        | ExprKind::Field(_, _)
        | ExprKind::Index(_, _) => {
            let (lvalue, _lvalue_type) = expr_to_lvalue(function, lhs, out)?;
            match lvalue {
                LValue::Local(local_index) => {
                    if op.is_some() {
                        local_get(function, local_index, out);
                    }
                    visit_expr(module, function, rhs, out)?;
                    if let Some(bop) = op {
                        out.add_op(StackOp::BinaryOp(
                            bop,
                            match lhs.typ {
                                ExprType::I32 => StackOpType::I32,
                                ExprType::I64 => StackOpType::I64,
                                ExprType::F32 => StackOpType::F32,
                                ExprType::F64 => StackOpType::F64,
                                _ => panic!("Invalid type for binary addition: {:?}", lhs.typ),
                            },
                        ));
                        // visit_binop(&lvalue_type, bop, &lhs.typ, &rhs.typ, out);
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
    _function: &FunctionBody,
    expr: &Expr,
    _out: &mut BlockBuilder,
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

fn visit_stack_op<'cu>(op: &StackOp, out: &mut InstructionBuilder) -> Result<(), CompilationError> {
    match op {
        StackOp::ConstVoid => {
            out.push_op(instr::OP_CONST_VOID);
        }

        StackOp::ConstBool(value) => {
            match value {
                true => out.push_op(instr::OP_CONST_TRUE),
                false => out.push_op(instr::OP_CONST_FALSE),
            };
        }

        StackOp::ConstI32(value) => {
            out.push_op(instr::OP_CONST_I32);
            out.push_immediate(*value);
        }

        StackOp::ConstI64(value) => {
            out.push_op(instr::OP_CONST_I64);
            out.push_immediate(*value);
        }

        StackOp::ConstF32(value) => {
            out.push_op(instr::OP_CONST_F32);
            out.push_immediate(*value);
        }

        StackOp::ConstF64(value) => {
            out.push_op(instr::OP_CONST_F64);
            out.push_immediate(*value);
        }

        StackOp::ConstStr(str) => {
            let str_len = str.len();
            if str_len > u16::MAX as usize {
                panic!("String constants longer than 64k are not supported");
            }

            out.push_op(instr::OP_CONST_STR);
            out.push_immediate(str_len as u16);
            out.push_immediate_string(str);
        }

        StackOp::Drop(n) => {
            todo!();
        }

        StackOp::LoadParam(index) => {
            out.push_op(instr::OP_LOAD_PARAM);
            out.push_immediate::<u16>(*index);
        }
        StackOp::LoadLocal(index) => {
            out.push_op(instr::OP_LOAD_LOCAL);
            out.push_immediate::<u16>(*index);
        }

        StackOp::StoreLocal(index) => {
            out.push_op(instr::OP_STORE_LOCAL);
            out.push_immediate::<u16>(*index);
        }

        StackOp::LoadField(index) => {
            out.push_op(instr::OP_LOAD_FIELD);
            out.push_immediate::<u16>(*index);
        }

        StackOp::LoadNativeProp(field_index) => {
            out.push_op(instr::OP_LOAD_NATIVE_PROP);
            out.push_immediate::<u16>(*field_index);
        }

        StackOp::LoadGlobal(index) => {
            out.push_op(instr::OP_LOAD_GLOBAL);
            out.push_immediate::<u16>(*index);
        }

        StackOp::BinaryOp(op, ty) => {
            visit_binop(*ty, *op, out);
        }

        StackOp::UnaryOp(op, _ty) => match op {
            UnaryOp::Not => todo!(),
            UnaryOp::Neg => todo!(),
            UnaryOp::BitNot => todo!(),
        },

        StackOp::CallScriptFunction { method, num_args } => {
            out.push_op(instr::OP_CALL);
            out.push_immediate::<u16>(*method);
            out.push_immediate::<u16>(*num_args);
        }

        StackOp::CallHostFunction { method, num_args } => {
            out.push_op(instr::OP_CALL_HOST_FUNCTION);
            out.push_immediate::<u16>(*method);
            out.push_immediate::<u16>(*num_args);
        }

        StackOp::CallHostMethod { method, num_args } => {
            out.push_op(instr::OP_CALL_HOST_METHOD);
            out.push_immediate::<u16>(*method);
            out.push_immediate::<u16>(*num_args);
        }
    }

    Ok(())
}

fn visit_binop(typ: StackOpType, op: BinaryOp, out: &mut InstructionBuilder) {
    match op {
        BinaryOp::Add => {
            match typ {
                StackOpType::I32 => out.push_op(instr::OP_ADD_I32),
                StackOpType::I64 => out.push_op(instr::OP_ADD_I64),
                StackOpType::F32 => out.push_op(instr::OP_ADD_F32),
                StackOpType::F64 => out.push_op(instr::OP_ADD_F64),
                _ => panic!("Invalid type for binary addition: {typ:?}"),
            };
        }

        BinaryOp::Sub => {
            match typ {
                StackOpType::I32 => out.push_op(instr::OP_SUB_I32),
                StackOpType::I64 => out.push_op(instr::OP_SUB_I64),
                StackOpType::F32 => out.push_op(instr::OP_SUB_F32),
                StackOpType::F64 => out.push_op(instr::OP_SUB_F64),
                _ => panic!("Invalid type for binary subtraction: {typ:?}"),
            };
        }

        BinaryOp::Mul => {
            match typ {
                StackOpType::I32 => out.push_op(instr::OP_MUL_I32),
                StackOpType::I64 => out.push_op(instr::OP_MUL_I64),
                StackOpType::F32 => out.push_op(instr::OP_MUL_F32),
                StackOpType::F64 => out.push_op(instr::OP_MUL_F64),
                _ => panic!("Invalid type for binary multiplication: {typ:?}"),
            };
        }

        BinaryOp::Div => {
            match typ {
                StackOpType::I32 => out.push_op(instr::OP_DIV_I32),
                StackOpType::I64 => out.push_op(instr::OP_DIV_I64),
                StackOpType::F32 => out.push_op(instr::OP_DIV_F32),
                StackOpType::F64 => out.push_op(instr::OP_DIV_F64),
                _ => panic!("Invalid type for binary division: {typ:?}"),
            };
        }

        BinaryOp::Mod => {
            match typ {
                StackOpType::I32 => out.push_op(instr::OP_REM_I32),
                StackOpType::I64 => out.push_op(instr::OP_REM_I64),
                _ => panic!("Invalid type for binary modulo: {typ:?}"),
            };
        }

        BinaryOp::LogAnd => {
            todo!();
        }

        BinaryOp::LogOr => {
            todo!();
        }

        BinaryOp::BitAnd => {
            match typ {
                StackOpType::I32 => out.push_op(instr::OP_BIT_AND_I32),
                StackOpType::I64 => out.push_op(instr::OP_BIT_AND_I64),
                _ => panic!("Invalid type for bitwise AND: {typ:?}"),
            };
        }

        BinaryOp::BitOr => {
            match typ {
                StackOpType::I32 => out.push_op(instr::OP_BIT_OR_I32),
                StackOpType::I64 => out.push_op(instr::OP_BIT_OR_I64),
                _ => panic!("Invalid type for bitwise OR: {typ:?}"),
            };
        }

        BinaryOp::BitXor => {
            match typ {
                StackOpType::I32 => out.push_op(instr::OP_BIT_XOR_I32),
                StackOpType::I64 => out.push_op(instr::OP_BIT_XOR_I64),
                _ => panic!("Invalid type for bitwise XOR: {typ:?}"),
            };
        }

        BinaryOp::Shl => {
            match typ {
                StackOpType::I32 => out.push_op(instr::OP_SHL),
                StackOpType::I64 => out.push_op(instr::OP_SHL),
                _ => panic!("Invalid type for bitwise shift left: {typ:?}"),
            };
        }

        BinaryOp::Shr => {
            match typ {
                StackOpType::I32 => out.push_op(instr::OP_SHR),
                StackOpType::I64 => out.push_op(instr::OP_SHR),
                _ => panic!("Invalid type for bitwise shift right: {typ:?}"),
            };
        }

        BinaryOp::Eq => {
            match typ {
                StackOpType::I32 => out.push_op(instr::OP_EQ_I32),
                StackOpType::I64 => out.push_op(instr::OP_EQ_I64),
                StackOpType::F32 => out.push_op(instr::OP_EQ_F32),
                StackOpType::F64 => out.push_op(instr::OP_EQ_F64),
                _ => panic!("Invalid type for equality comparison: {typ:?}"),
            };
        }

        BinaryOp::Ne => {
            match typ {
                StackOpType::I32 => out.push_op(instr::OP_NE_I32),
                StackOpType::I64 => out.push_op(instr::OP_NE_I64),
                StackOpType::F32 => out.push_op(instr::OP_NE_F32),
                StackOpType::F64 => out.push_op(instr::OP_NE_F64),
                _ => panic!("Invalid type for inequality comparison: {typ:?}"),
            };
        }

        BinaryOp::Lt => {
            match typ {
                StackOpType::I32 => out.push_op(instr::OP_LT_I32),
                StackOpType::I64 => out.push_op(instr::OP_LT_I64),
                StackOpType::F32 => out.push_op(instr::OP_LT_F32),
                StackOpType::F64 => out.push_op(instr::OP_LT_F64),
                _ => panic!("Invalid type for less-than comparison: {typ:?}"),
            };
        }

        BinaryOp::Le => {
            match typ {
                StackOpType::I32 => out.push_op(instr::OP_LE_I32),
                StackOpType::I64 => out.push_op(instr::OP_LE_I64),
                StackOpType::F32 => out.push_op(instr::OP_LE_F32),
                StackOpType::F64 => out.push_op(instr::OP_LE_F64),
                _ => panic!("Invalid type for less-than-or-equal comparison: {typ:?}"),
            };
        }

        BinaryOp::Gt => {
            match typ {
                StackOpType::I32 => out.push_op(instr::OP_GT_I32),
                StackOpType::I64 => out.push_op(instr::OP_GT_I64),
                StackOpType::F32 => out.push_op(instr::OP_GT_F32),
                StackOpType::F64 => out.push_op(instr::OP_GT_F64),
                _ => panic!("Invalid type for greater-than comparison: {typ:?}"),
            };
        }

        BinaryOp::Ge => {
            match typ {
                StackOpType::I32 => out.push_op(instr::OP_GE_I32),
                StackOpType::I64 => out.push_op(instr::OP_GE_I64),
                StackOpType::F32 => out.push_op(instr::OP_GE_F32),
                StackOpType::F64 => out.push_op(instr::OP_GE_F64),
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
    fn test_visit_expr() {
        let mut builder = BlockBuilder::default();
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

        builder.create_initial_block();
        visit_expr(&module, &function, &expr, &mut builder).unwrap();

        assert_eq!(builder.blocks.len(), 1);

        let block = &builder.blocks[0];
        assert_eq!(
            block.ops,
            vec![
                StackOp::ConstI32(2),
                StackOp::ConstI32(2),
                StackOp::BinaryOp(BinaryOp::Add, StackOpType::I32),
            ]
        );
    }
}
