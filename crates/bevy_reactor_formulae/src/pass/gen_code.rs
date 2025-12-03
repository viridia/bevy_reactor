use core::result;

use bevy::scene::ron::de;

use crate::{
    ast::{ASTNode, NodeKind},
    compiler::{CompilationError, CompiledFunction, CompiledModule, FunctionBody, ModuleExprs},
    decl::{self, ParamDecl, Scope},
    expr::{Expr, ExprKind},
    expr_type::ExprType,
    instr::{self, InstructionBuilder, OP_CONST_I32},
    oper::{BinaryOp, UnaryOp},
};

// pub struct CodeGenerator {
//     // elements: ElementSection,
//     // next_type_index: u32,
//     // next_data_index: u32,
//     params: Vec<ParamDecl>,
//     // locals: Vec<LocalDecl>,
//     type_string: Option<u32>,
// }

// impl Default for CodeGenerator {
//     fn default() -> Self {
//         Self {
//             // elements: ElementSection::new(),
//             // next_type_index: 0,
//             // next_data_index: 0,
//             params: Vec::new(),
//             // locals: Vec::new(),
//             type_string: None,
//         }
//     }
// }

// impl CodeGenerator {
// pub fn next_type_index(&mut self) -> u32 {
//     let index = self.next_type_index;
//     self.next_type_index += 1;
//     index
// }

// pub fn next_data_index(&mut self) -> u32 {
//     let index = self.next_data_index;
//     self.next_data_index += 1;
//     index
// }

// fn gen_val_types(&mut self, typ: &ExprType, out: &mut Vec<ValType>) {
//     match &typ {
//         ExprType::None | ExprType::Void => {}
//         ExprType::IUnsized | ExprType::Infer(_) => unreachable!(),
//         ExprType::Boolean => {
//             out.push(ValType::I32);
//         }
//         ExprType::I32 => {
//             out.push(ValType::I32);
//         }
//         ExprType::I64 => {
//             out.push(ValType::I64);
//         }
//         ExprType::F32 => {
//             out.push(ValType::F32);
//         }
//         ExprType::F64 => {
//             out.push(ValType::F64);
//         }
//         ExprType::String => {
//             out.push(ValType::Ref(RefType {
//                 nullable: false,
//                 heap_type: HeapType::Concrete(self.get_string_type()),
//             }));
//         }
//         ExprType::Array(_element) => {
//             todo!();
//             // out.push(ValType::Ref(RefType {
//             //     nullable: false,
//             //     heap_type: HeapType::A,
//             // }));
//             // self.gen_val_types(element, out);
//         }
//         ExprType::Struct(stype) => {
//             if stype.is_record {
//                 out.push(ValType::I32);
//             } else {
//                 for field in &stype.fields {
//                     self.gen_val_types(&field.typ, out);
//                 }
//             }
//         }
//         ExprType::TupleStruct(stype) => {
//             if stype.is_record {
//                 out.push(ValType::I32);
//             } else {
//                 for typ in &stype.fields {
//                     self.gen_val_types(typ, out);
//                 }
//             }
//         }
//         ExprType::Tuple(members) => {
//             for member in members.iter() {
//                 self.gen_val_types(member, out);
//             }
//         }
//         ExprType::Function(_ftype) => {
//             todo!();
//             // for param in &ftype.params {
//             //     self.gen_val_types(param, out);
//             // }
//             // for ret in &ftype.ret {
//             //     self.gen_val_types(ret, out);
//             // }
//         }
//     }
// }

// pub fn get_string_type(&mut self) -> u32 {
//     if let Some(index) = self.type_string {
//         index
//     } else {
//         self.types.ty().array(&StorageType::I8, false);
//         let s = self.next_type_index();
//         self.type_names.append(s, "String");
//         self.type_string = Some(s);
//         s
//     }
// }
// }

pub(crate) fn gen_module<'content>(
    module: &mut CompiledModule,
    module_exprs: &'content ModuleExprs<'content>,
) -> Result<(), CompilationError> {
    // let mut generator = CodeGenerator::default();
    module
        .functions
        .resize_with(module_exprs.functions.len(), || CompiledFunction {
            code: Vec::new(),
        });

    for (_, decl) in module.module_decls.iter() {
        match &decl.kind {
            decl::DeclKind::Global {
                typ: _,
                is_const: _,
                index: _,
            } => todo!(),
            decl::DeclKind::Local {
                typ: _,
                is_const: _,
                index: _,
            } => unreachable!(),
            decl::DeclKind::Function {
                params: _,
                ret: _,
                is_native: _,
                index,
            } => {
                let mut builder = instr::InstructionBuilder::default();
                let function = &module_exprs.functions[*index];
                if let Some(body) = function.body {
                    gen_expr(module, function, body, &mut builder).unwrap();
                    if !body.typ.is_void() {
                        builder.push_op(instr::OP_RET);
                    }
                }
                module.functions[*index].code = builder.inner();
            }
            _ => {}
        }
    }

    // for sd in unit.decls.structs.iter() {
    //     if sd.typ.is_record {
    //         // let name_str = unit.symbols.resolve(sd.name);
    //         // let mut fields = vec![];
    //         // for field in &sd.fields {
    //         //     fields.push(FieldType {
    //         //         typ: generator.gen_type(&field.typ),
    //         //         mutable: field.mutable,
    //         //     });
    //         // }
    //         // let type_index = generator.next_type_index();
    //         // generator
    //         //     .type_names
    //         //     .append(type_index, format!("{}.type", name_str).as_str());
    //         // generator.types.ty().struct_type(fields);
    //         // generator
    //         //     .imports
    //         //     .import("host", &name_str, EntityType::Struct(type_index));
    //     }
    // }

    // for fd in unit.decls.functions.iter() {
    //     let name_str = unit.decls.symbols.resolve(fd.name);
    //     let mut ret_types: Vec<ValType> = Vec::new();
    //     if !fd.typ.ret.is_void() {
    //         generator.gen_val_types(&fd.typ.ret, &mut ret_types);
    //     };

    //     let mut param_types: Vec<ValType> = Vec::new();
    //     for param in &fd.typ.params {
    //         generator.gen_val_types(&param.typ, &mut param_types);
    //     }

    //     let type_index = generator.next_type_index();
    //     generator
    //         .function_names
    //         .append(fd.function_index as u32, &name_str);
    //     generator
    //         .type_names
    //         .append(type_index, format!("{}.type", name_str).as_str());
    //     generator.types.ty().function(param_types, ret_types);
    //     if fd.is_native {
    //         generator
    //             .imports
    //             .import("host", &name_str, EntityType::Function(type_index));
    //     } else {
    //         generator.functions.function(type_index);
    //         if fd.visibility == decl::DeclVisibility::Public {
    //             generator
    //                 .exports
    //                 .export(&name_str, ExportKind::Func, fd.function_index as u32);
    //         }
    //         let mut locals: Vec<ValType> = vec![];
    //         for local in &fd.locals {
    //             generator.gen_val_types(&local.typ, &mut locals);
    //         }
    //         let mut locals_compressed: Vec<(u32, ValType)> = Vec::new();
    //         if !locals.is_empty() {
    //             let mut current_type = &locals[0];
    //             let mut count = 1;
    //             for local in locals.iter().skip(1) {
    //                 if local == current_type {
    //                     count += 1;
    //                 } else {
    //                     locals_compressed.push((count, *current_type));
    //                     current_type = local;
    //                     count = 1;
    //                 }
    //             }
    //             locals_compressed.push((count, *current_type));
    //         }
    //         let mut f = Function::new(locals_compressed);
    //         generator.params.clone_from(&fd.typ.params);
    //         generator.locals.clone_from(&fd.locals);
    //         let mut local_offset = 0;
    //         for param in generator.params.iter_mut() {
    //             param.local_index = local_offset;
    //             local_offset += param.typ.value_count();
    //         }
    //         for local in generator.locals.iter_mut() {
    //             local.local_index = local_offset;
    //             local_offset += local.typ.value_count();
    //         }
    //         gen_expr(unit, &mut generator, &fd.body, &mut f)?;
    //         if !fd.body.typ.is_void() {
    //             f.instruction(&Instruction::Return);
    //         }
    //         f.instruction(&Instruction::End);
    //         generator.codes.function(&f);
    //     }
    // }

    // let mut names = NameSection::new();
    // names.module(unit.filename());

    // if !generator.type_names.is_empty() {
    //     names.types(&generator.type_names);
    // }

    // names.locals(&generator.local_names);

    // if !generator.function_names.is_empty() {
    //     names.functions(&generator.function_names);
    // }

    // unit.module.section(&names);
    // unit.module.section(&generator.types);
    // if !generator.imports.is_empty() {
    //     unit.module.section(&generator.imports);
    // }
    // unit.module.section(&generator.functions);
    // unit.module.section(&generator.exports);
    // if generator.next_data_index > 0 {
    //     unit.module.section(&DataCountSection {
    //         count: generator.next_data_index,
    //     });
    // }
    // unit.module.section(&generator.codes);
    // if generator.next_data_index > 0 {
    //     unit.module.section(&generator.data);
    // }

    // println!(
    //     "{}",
    //     wasmprinter::print_bytes(unit.module.as_slice()).unwrap()
    // );
    // wasmparser::validate(unit.module.as_slice()).unwrap();
    Ok(())
}

fn gen_expr<'cu>(
    module: &'cu CompiledModule,
    // module_contents: &'cu mut ModuleContents<'cu>,
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
        // ExprKind::ConstString(symbol) => {
        //     let string = unit.decls.symbols.resolve(symbol);
        //     let bytes = string.as_bytes();
        //     let array_data_index = generator.next_data_index();
        //     generator.data.passive(bytes.iter().copied());
        //     out.instruction(&Instruction::I32Const(0));
        //     out.instruction(&Instruction::I32Const(bytes.len() as i32));
        //     out.instruction(&Instruction::ArrayNewData {
        //         array_type_index: generator.get_string_type(),
        //         array_data_index,
        //     });
        // }
        // ExprKind::FunctionRef(index) => {
        //     panic!("Cannot codegen function reference: {:?}", index);
        // }
        // ExprKind::ParamRef(index) => {
        //     let local = &generator.params[index];
        //     local_get(local.local_index, &local.typ, out);
        // }
        // ExprKind::LocalRef(index) => {
        //     let local = &generator.locals[index];
        //     local_get(local.local_index, &local.typ, out);
        // }
        // ExprKind::Field(ref base, field_index) => {
        //     // gen_expr(unit, generator, base, out)?;
        //     let field = match &base.typ {
        //         ExprType::Struct(stype) => &stype.fields[field_index],
        //         _ => panic!("Invalid field access: {:?}", base.typ),
        //     };
        //     match base.kind {
        //         ExprKind::LocalRef(index) => {
        //             let local = &generator.locals[index];
        //             local_get(local.local_index + field.index, &field.typ, out);
        //         }
        //         _ => todo!(),
        //     }
        // }
        // ExprKind::Index(ref base, _field_index) => {
        //     gen_expr(unit, generator, base, out)?;
        //     todo!();
        // }
        // ExprKind::GlobalRef(index) => {
        //     out.instruction(&Instruction::GlobalGet(index as u32));
        // }
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
            gen_binop(&expr.typ, op, out);
        }

        // ExprKind::Assign { ref lhs, ref rhs } => {
        //     // gen_expr(unit, generator, rhs, out)?;
        //     gen_assign(unit, generator, lhs, rhs, None, out)?;
        // }

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
        // ExprKind::Call(ref func, ref args) => {
        //     for arg in args {
        //         gen_expr(unit, generator, arg, out)?;
        //     }
        //     if let ExprKind::FunctionRef(index) = func.kind {
        //         // println!("Call function: {}", index);
        //         out.instruction(&Instruction::Call(index as u32));
        //     } else {
        //         panic!("Invalid function reference: {:?}", func);
        //     }
        // }
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

        _ => todo!("Unimplemented {:?}", expr.kind),
    }

    Ok(())
}

// fn local_get(mut local_index: usize, typ: &ExprType, out: &mut wasm_encoder::Function) {
//     match typ {
//         ExprType::None | ExprType::Void => {}
//         ExprType::IUnsized | ExprType::Infer(_) => unreachable!(),
//         ExprType::Boolean | ExprType::I32 | ExprType::I64 | ExprType::F32 | ExprType::F64 => {
//             out.instruction(&Instruction::LocalGet(local_index as u32));
//         }
//         ExprType::String => {
//             out.instruction(&Instruction::LocalGet(local_index as u32));
//         }
//         ExprType::Array(_element) => {
//             todo!();
//         }
//         ExprType::Struct(stype) => {
//             if stype.is_record {
//                 out.instruction(&Instruction::LocalGet(local_index as u32));
//             } else {
//                 for field in &stype.fields {
//                     local_get(local_index, &field.typ, out);
//                     local_index += field.typ.value_count();
//                 }
//             }
//         }
//         ExprType::TupleStruct(stype) => {
//             if stype.is_record {
//                 out.instruction(&Instruction::LocalGet(local_index as u32));
//             } else {
//                 for typ in &stype.fields {
//                     local_get(local_index, typ, out);
//                     local_index += typ.value_count();
//                 }
//             }
//         }
//         ExprType::Tuple(members) => {
//             for member in members.iter() {
//                 local_get(local_index, member, out);
//                 local_index += member.value_count();
//             }
//         }
//         ExprType::Function(_ftype) => {
//             todo!();
//         }
//     }
// }

// fn local_set(mut local_index: usize, typ: &ExprType, out: &mut wasm_encoder::Function) {
//     match typ {
//         ExprType::None | ExprType::Void => {}
//         ExprType::IUnsized | ExprType::Infer(_) => unreachable!(),
//         ExprType::Boolean | ExprType::I32 | ExprType::I64 | ExprType::F32 | ExprType::F64 => {
//             out.instruction(&Instruction::LocalSet(local_index as u32));
//         }
//         ExprType::String => {
//             out.instruction(&Instruction::LocalSet(local_index as u32));
//         }
//         ExprType::Array(_element) => {
//             todo!();
//         }
//         ExprType::Struct(stype) => {
//             if stype.is_record {
//                 out.instruction(&Instruction::LocalSet(local_index as u32));
//             } else {
//                 for field in stype.fields.iter().rev() {
//                     local_set(local_index, &field.typ, out);
//                     local_index += field.typ.value_count();
//                 }
//             }
//         }
//         ExprType::TupleStruct(stype) => {
//             if stype.is_record {
//                 out.instruction(&Instruction::LocalSet(local_index as u32));
//             } else {
//                 for typ in stype.fields.iter().rev() {
//                     local_set(local_index, typ, out);
//                     local_index += typ.value_count();
//                 }
//             }
//         }
//         ExprType::Tuple(members) => {
//             for member in members.iter().rev() {
//                 local_set(local_index, member, out);
//                 local_index += member.value_count();
//             }
//         }
//         ExprType::Function(_ftype) => {
//             todo!();
//         }
//     }
// }

// fn gen_assign(
//     unit: &CompilationUnit,
//     generator: &mut CodeGenerator,
//     lvalue: &Expr,
//     rvalue: &Expr,
//     op: Option<BinaryOp>,
//     out: &mut wasm_encoder::Function,
// ) -> Result<(), CompilationError> {
//     match lvalue.kind {
//         ExprKind::ParamRef(_)
//         | ExprKind::LocalRef(_)
//         | ExprKind::GlobalRef(_)
//         | ExprKind::Field(_, _)
//         | ExprKind::Index(_, _) => {
//             let (lvalue, lvalue_type) = expr_to_lvalue(unit, generator, lvalue, out)?;
//             match lvalue {
//                 LValue::Local(local_index) => {
//                     if op.is_some() {
//                         local_get(local_index, &lvalue_type, out);
//                     }
//                     gen_expr(unit, generator, rvalue, out)?;
//                     if let Some(bop) = op {
//                         gen_binop(&lvalue_type, bop, out);
//                     }
//                     local_set(local_index, &lvalue_type, out);
//                 }
//                 _ => todo!(),
//             }
//         }
//         _ => return Err(CompilationError::InvalidAssignmentTarget(lvalue.location)),
//     }

//     Ok(())
// }

// enum LValue {
//     Local(usize),
//     #[allow(unused)]
//     Global(usize),
//     // Memory?
//     // Reference?
// }

// fn expr_to_lvalue(
//     unit: &CompilationUnit,
//     generator: &mut CodeGenerator,
//     expr: &Expr,
//     out: &mut wasm_encoder::Function,
// ) -> Result<(LValue, ExprType), CompilationError> {
//     match expr.kind {
//         ExprKind::ParamRef(index) => {
//             let param = &generator.params[index];
//             Ok((LValue::Local(param.local_index), param.typ.clone()))
//         }
//         ExprKind::LocalRef(index) => {
//             let local = &generator.locals[index];
//             Ok((LValue::Local(local.local_index), local.typ.clone()))
//         }
//         ExprKind::GlobalRef(_index) => todo!(),
//         ExprKind::Field(ref base, field_index) => {
//             let (base_lvalue, base_type) = expr_to_lvalue(unit, generator, base, out)?;
//             match base_type {
//                 ExprType::Struct(stype) => {
//                     if stype.is_record {
//                         gen_expr(unit, generator, base, out)?;
//                         todo!();
//                     } else {
//                         let field = &stype.fields[field_index];
//                         match base_lvalue {
//                             LValue::Local(local_index) => {
//                                 Ok((LValue::Local(local_index + field.index), field.typ.clone()))
//                             }
//                             _ => todo!(),
//                         }
//                     }
//                 }
//                 _ => panic!("Invalid field access: {:?}", base_type),
//             }
//         }
//         ExprKind::Index(ref _expr, _) => todo!(),
//         _ => panic!("Invalid lvalue: {:?}", expr),
//     }
// }

fn gen_binop(typ: &ExprType, op: BinaryOp, out: &mut InstructionBuilder) {
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
            match typ {
                ExprType::I32 => out.push_op(instr::OP_EQUAL),
                ExprType::I64 => out.push_op(instr::OP_EQUAL),
                ExprType::F32 => out.push_op(instr::OP_EQUAL),
                ExprType::F64 => out.push_op(instr::OP_EQUAL),
                _ => panic!("Invalid type for equality comparison: {typ:?}"),
            };
        }
        BinaryOp::Ne => {
            match typ {
                ExprType::I32 => out.push_op(instr::OP_NOT_EQUAL),
                ExprType::I64 => out.push_op(instr::OP_NOT_EQUAL),
                ExprType::F32 => out.push_op(instr::OP_NOT_EQUAL),
                ExprType::F64 => out.push_op(instr::OP_NOT_EQUAL),
                _ => panic!("Invalid type for inequality comparison: {typ:?}"),
            };
        }
        BinaryOp::Lt => {
            match typ {
                ExprType::I32 => out.push_op(instr::OP_LESS),
                ExprType::I64 => out.push_op(instr::OP_LESS),
                ExprType::F32 => out.push_op(instr::OP_LESS),
                ExprType::F64 => out.push_op(instr::OP_LESS),
                _ => panic!("Invalid type for less-than comparison: {typ:?}"),
            };
        }
        BinaryOp::Le => {
            match typ {
                ExprType::I32 => out.push_op(instr::OP_LESS_EQUAL),
                ExprType::I64 => out.push_op(instr::OP_LESS_EQUAL),
                ExprType::F32 => out.push_op(instr::OP_LESS_EQUAL),
                ExprType::F64 => out.push_op(instr::OP_LESS_EQUAL),
                _ => panic!("Invalid type for less-than-or-equal comparison: {typ:?}"),
            };
        }
        BinaryOp::Gt => {
            match typ {
                ExprType::I32 => out.push_op(instr::OP_GREATER),
                ExprType::I64 => out.push_op(instr::OP_GREATER),
                ExprType::F32 => out.push_op(instr::OP_GREATER),
                ExprType::F64 => out.push_op(instr::OP_GREATER),
                _ => panic!("Invalid type for greater-than comparison: {typ:?}"),
            };
        }
        BinaryOp::Ge => {
            match typ {
                ExprType::I32 => out.push_op(instr::OP_GREATER_EQUAL),
                ExprType::I64 => out.push_op(instr::OP_GREATER_EQUAL),
                ExprType::F32 => out.push_op(instr::OP_GREATER_EQUAL),
                ExprType::F64 => out.push_op(instr::OP_GREATER_EQUAL),
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
        let module = CompiledModule::default();
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
