use thiserror::Error;

#[non_exhaustive]
#[derive(Debug, Error)]
enum EvalError {
    #[error("Invalid assignment")]
    InvalidAssignment,
}

#[derive(Debug, PartialEq, Clone)]
enum Value {
    Void,
    I32(i32),
    F64(f64),
    // String
    // InternedString?
    // Struct?
    // Tuple?
    // Entity
    // Entity+Component
    // ComponentRef(Entity, ComponentId),
    // Resource
}

struct FnContext<'f> {
    /// Array of function local vars.
    pub(crate) locals: &'f mut [Value],
}

#[derive(Debug)]
enum ExprKind<'a> {
    // Literal values
    Const(Value),
    // ConstString(decl::Symbol),

    // Variable references
    LocalRef(usize),

    Assign {
        lhs: &'a Expr<'a>,
        rhs: &'a Expr<'a>,
    },
}

/// Expression node.
#[derive(Debug)]
struct Expr<'a> {
    // pub(crate) location: TokenLocation,
    kind: ExprKind<'a>,
    // pub(crate) typ: Type,
}

impl<'a> Expr<'a> {
    /// Evaluate an expression
    fn eval(&self, fn_context: &mut FnContext) -> Result<Value, EvalError> {
        match &self.kind {
            ExprKind::Const(value) => Ok(value.clone()),
            ExprKind::LocalRef(index) => Ok(fn_context.locals[*index].clone()),
            ExprKind::Assign { lhs, rhs } => {
                let val = rhs.eval(fn_context)?;
                // let reflect = lhs.as_reflect_mut(fn_context)?;
                // reflect.apply(val.as_reflect());
                lhs.store(fn_context, val)?;
                Ok(Value::Void)
            }
        }
    }

    /// Store a value into an expression
    fn store(&self, fn_context: &mut FnContext, value: Value) -> Result<(), EvalError> {
        match self.kind {
            ExprKind::LocalRef(index) => {
                fn_context.locals[index] = value;
                Ok(())
            }
            _ => Err(EvalError::InvalidAssignment),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_assign_local() {
        let mut locals: Vec<Value> = vec![Value::I32(10)];
        let mut fn_context = FnContext {
            locals: &mut locals[..],
        };
        let lhs = Expr {
            kind: ExprKind::LocalRef(0),
        };
        let rhs = Expr {
            kind: ExprKind::Const(Value::F64(1.0)),
        };
        let expr = Expr {
            kind: ExprKind::Assign {
                lhs: &lhs,
                rhs: &rhs,
            },
        };
        let result = expr.eval(&mut fn_context).unwrap();
        assert!(matches!(result, Value::Void));
        assert_eq!(locals[0], Value::F64(1.0));
    }
}
