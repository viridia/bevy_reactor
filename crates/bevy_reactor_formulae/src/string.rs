//! String type and methods.

use std::sync::OnceLock;

use crate::{VM, Value, decl::ObjectMembers, expr_type::ExprType, vm::VMError};

static STRING_METHODS: OnceLock<ObjectMembers> = OnceLock::new();

pub(crate) fn get_string_methods() -> &'static ObjectMembers {
    STRING_METHODS.get_or_init(|| {
        let mut table = ObjectMembers::default();
        table.add_method("len", string_len, Vec::new(), ExprType::I32);
        table
    })
}

/// string.len()
fn string_len(_vm: &VM, args: &[Value]) -> Result<Value, VMError> {
    assert_eq!(args.len(), 1);
    if let Value::String(str) = &args[0] {
        Ok(Value::I32(str.len() as i32))
    } else {
        Err(VMError::MismatchedTypes(
            ExprType::String,
            args[0].value_type(),
        ))
    }
}

// fn string_len_native(str: &Arc<String>) -> Result<i32, VMError> {
//     Ok(str.len() as i32)
// }
