use core::result;

use bevy::{math::ops::exp, scene::ron::de};

use crate::{
    Module,
    ast::{ASTNode, NodeKind},
    bblock::{BasicBlock, BlockTerminator, StackOp},
    compiler::{CompilationError, CompiledFunction, ModuleExprs},
    decl::{self, FunctionParam, Scope},
    expr::{Expr, ExprKind, FunctionBody},
    expr_type::ExprType,
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
        self.blocks.push(BasicBlock {
            id,
            ..Default::default()
        });
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

pub(crate) fn build_bblocks<'content>(
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
                    builder.add_op(StackOp::Return);
                }
                // module.functions[*index].code = builder.inner();
                // module.functions[*index].num_locals = function.locals.len();
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
            out.add_op(StackOp::BinaryOp(op));
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
                out.add_op(StackOp::CallModuleFunction {
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
                        out.add_op(StackOp::BinaryOp(bop));
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
                StackOp::BinaryOp(BinaryOp::Add),
            ]
        );
    }
}
