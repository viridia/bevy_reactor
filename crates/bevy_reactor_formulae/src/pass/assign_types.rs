use core::result;

use crate::{
    compiler::CompilationError,
    decl,
    expr::{Expr, ExprKind},
    expr_type::ExprType,
    oper::{BinaryOp, UnaryOp},
};

use super::type_inference::TypeInference;

pub(crate) fn assign_types<'e>(
    expr: &mut Expr<'e>,
    inference: &TypeInference,
) -> Result<(), CompilationError> {
    match &mut expr.kind {
        ExprKind::Empty => {}
        ExprKind::ConstInteger(_) => {
            inference.replace_type_vars(&mut expr.typ);
        }
        ExprKind::ConstFloat(_) => {
            inference.replace_type_vars(&mut expr.typ);
        }
        ExprKind::ConstBool(_) => {}
        ExprKind::ConstString(_) => {}
        ExprKind::FunctionRef(_) => {}
        ExprKind::LocalRef(_) | ExprKind::GlobalRef(_) | ExprKind::ParamRef(_) => {}
        ExprKind::Field(base, _) | ExprKind::Index(base, _) | ExprKind::EntityProp(base, _) => {
            assign_types(base, inference)?;
        }
        ExprKind::LocalDecl(_, init) => {
            if let Some(init) = init {
                assign_types(init, inference)?;
            }
        }
        ExprKind::BinaryExpr { op, lhs, rhs } => {
            assign_types(lhs, inference)?;
            assign_types(rhs, inference)?;
            match op {
                BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
                    let ty = inference.substitute(&expr.typ);
                    match ty {
                        ExprType::I32 | ExprType::I64 | ExprType::F32 | ExprType::F64 => {
                            expr.typ = ty;
                        }

                        _ => {
                            return Err(CompilationError::InvalidBinaryOpType(
                                expr.location,
                                *op,
                                lhs.typ.clone(),
                                rhs.typ.clone(),
                            ));
                        }
                    }
                }

                BinaryOp::LogAnd | BinaryOp::LogOr => todo!(),

                BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor => {
                    let ty = inference.substitute(&expr.typ);
                    match ty {
                        ExprType::I32 | ExprType::I64 => {
                            expr.typ = ty;
                        }

                        _ => {
                            return Err(CompilationError::InvalidBinaryOpType(
                                expr.location,
                                *op,
                                lhs.typ.clone(),
                                rhs.typ.clone(),
                            ));
                        }
                    }
                }
                BinaryOp::Shl | BinaryOp::Shr => todo!(),

                // I don't think we need to do anything for these.
                BinaryOp::Eq | BinaryOp::Ne => {}
                BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => {}
            }
        }

        ExprKind::UnaryExpr { op, arg } => {
            assign_types(arg, inference)?;
            match op {
                UnaryOp::Not => todo!(),
                UnaryOp::Neg => todo!(),
                UnaryOp::BitNot => todo!(),
            }
        }

        ExprKind::Assign { lhs, rhs } => {
            assign_types(lhs, inference)?;
            assign_types(rhs, inference)?;
        }

        ExprKind::AssignOp { op, lhs, rhs } => {
            assign_types(lhs, inference)?;
            assign_types(rhs, inference)?;
            match op {
                BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
                    let ty = inference.substitute(&lhs.typ);
                    match ty {
                        ExprType::I32 | ExprType::I64 | ExprType::F32 | ExprType::F64 => {
                            // expr.typ = ty;
                        }

                        _ => {
                            return Err(CompilationError::InvalidBinaryOpType(
                                expr.location,
                                *op,
                                lhs.typ.clone(),
                                rhs.typ.clone(),
                            ));
                        }
                    }
                }

                BinaryOp::LogAnd | BinaryOp::LogOr => todo!(),

                BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor => {
                    let ty = inference.substitute(&expr.typ);
                    match ty {
                        ExprType::I32 | ExprType::I64 => {
                            expr.typ = ty;
                        }

                        _ => {
                            return Err(CompilationError::InvalidBinaryOpType(
                                expr.location,
                                *op,
                                lhs.typ.clone(),
                                rhs.typ.clone(),
                            ));
                        }
                    }
                }
                BinaryOp::Shl | BinaryOp::Shr => todo!(),

                BinaryOp::Eq | BinaryOp::Ne => todo!(),

                BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => todo!(),
            }
        }

        // No need to traverse here, this has already been done.
        ExprKind::Cast(_arg) => {}
        ExprKind::Call(func, args) => {
            assign_types(func, inference)?;
            for arg in args.iter_mut() {
                assign_types(arg, inference)?;
            }
        }

        ExprKind::Block(stmts, result) => {
            for stmt in stmts.iter_mut() {
                assign_types(stmt, inference)?;
            }
            if let Some(result) = result {
                assign_types(result, inference)?;
            }
        }

        ExprKind::If {
            test,
            then_branch,
            else_branch,
        } => {
            assign_types(test, inference)?;
            assign_types(then_branch, inference)?;
            if let Some(e) = else_branch {
                assign_types(e, inference)?;
            }
        }
    }
    Ok(())
}
