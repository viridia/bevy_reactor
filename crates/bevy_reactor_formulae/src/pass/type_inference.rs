use std::sync::Arc;

use bevy::platform::collections::HashMap;

use crate::{
    compiler::CompilationError, expr_type::ExprType, expr_type::TypeVarId, location::TokenLocation,
};

#[derive(Debug)]
struct Constraint {
    left: ExprType,
    right: ExprType,
    location: TokenLocation,
    // reason: ConstraintReason,
}

#[derive(Default)]
pub(crate) struct TypeInference {
    /// Equivalence constraints.
    constraints: Vec<Constraint>,

    /// Next type variable ID.
    next_typevar_id: usize,

    /// ExprType variable substitutions.
    pub(crate) substitutions: HashMap<TypeVarId, ExprType>,
}

impl TypeInference {
    pub(crate) fn fresh_typevar(&mut self) -> ExprType {
        let id = self.next_typevar_id;
        self.next_typevar_id += 1;
        ExprType::Infer(TypeVarId(id))
    }

    pub(crate) fn add_constraint(
        &mut self,
        left: ExprType,
        right: ExprType,
        location: TokenLocation,
    ) {
        self.constraints.push(Constraint {
            left,
            right,
            location,
        });
    }

    pub(crate) fn replace_type_vars(&self, ty: &mut ExprType) {
        *ty = self.substitute(ty);
    }

    pub(crate) fn substitute(&self, ty: &ExprType) -> ExprType {
        match ty {
            ExprType::Infer(id) => {
                if let Some(substituted_type) = self.substitutions.get(id) {
                    self.substitute(substituted_type)
                } else {
                    ty.clone()
                }
            }

            // ExprType::Tuple(types) => {
            //     // TODO: Return the original type if no substitutions are made.
            //     let mut new_types = Vec::with_capacity(types.len());
            //     for t in types.iter() {
            //         new_types.push(self.substitute(t));
            //     }
            //     ExprType::Tuple(Arc::from(new_types))
            // }

            // // TODO: Return the original type if no substitutions are made.
            // ExprType::Array(ty) => ExprType::Array(Arc::new(self.substitute(ty))),
            _ => ty.clone(),
        }
    }

    fn occurs_check(&self, var_id: TypeVarId, ty: &ExprType) -> bool {
        match ty {
            ExprType::Infer(id) => {
                if *id == var_id {
                    return true;
                }
                // Check if this type variable has a substitution
                if let Some(substituted_type) = self.substitutions.get(id) {
                    return self.occurs_check(var_id, substituted_type);
                }
                false
            }
            _ => false,
        }
    }

    fn unify(
        &mut self,
        t1: &ExprType,
        t2: &ExprType,
        location: TokenLocation,
    ) -> Result<(), CompilationError> {
        let t1 = self.substitute(t1);
        let t2 = self.substitute(t2);

        if t1 == t2 {
            return Ok(());
        }

        // println!("Unifying {:?} and {:?} at {:?}", t1, t2, location);
        match (&t1, &t2) {
            (ExprType::Infer(id), ty) | (ty, ExprType::Infer(id)) => {
                if self.occurs_check(*id, ty) {
                    return Err(CompilationError::RecursiveType(location, ty.clone()));
                } else {
                    self.substitutions.insert(*id, ty.clone());
                }
                Ok(())
            }

            (ExprType::I32, ExprType::IUnsized) | (ExprType::IUnsized, ExprType::I32) => Ok(()),
            (ExprType::I64, ExprType::IUnsized) | (ExprType::IUnsized, ExprType::I64) => Ok(()),

            // (ExprType::Tuple(types1), ExprType::Tuple(types2)) => {
            //     if types1.len() != types2.len() {
            //         return Err(CompilationError::MismatchedTypes(
            //             location,
            //             t1.clone(),
            //             t2.clone(),
            //         ));
            //     }
            //     for (t1, t2) in types1.iter().zip(types2.iter()) {
            //         self.unify(t1, t2, location)?;
            //     }
            //     Ok(())
            // }

            // (ExprType::Array(t1), ExprType::Array(t2)) => self.unify(t1, t2, location),
            _ => Err(CompilationError::MismatchedTypes(
                location,
                t1.clone(),
                t2.clone(),
            )),
        }
    }

    pub(crate) fn solve_constraints(&mut self) -> Result<(), CompilationError> {
        self.constraints.reverse();
        while let Some(Constraint {
            left: t1,
            right: t2,
            location,
        }) = self.constraints.pop()
        {
            self.unify(&t1, &t2, location)?;
        }
        Ok(())
    }
}
