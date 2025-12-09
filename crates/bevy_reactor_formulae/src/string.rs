//! String type and methods.

// use std::sync::OnceLock;

use crate::{HostState, VM, Value, expr_type::ExprType, vm::VMError};

// static STRING_METHODS: OnceLock<ObjectMembers> = OnceLock::new();

// pub(crate) fn get_string_methods() -> &'static ObjectMembers {
//     STRING_METHODS.get_or_init(|| {
//         let mut table = ObjectMembers::default();
//         table.add_method("len", string_len, Vec::new(), ExprType::I32);
//         table
//     })
// }

/// string.len()
fn string_len(vm: &VM, args: &[Value]) -> Result<Value, VMError> {
    assert_eq!(args.len(), 1);
    if let Value::String(str) = &args[0] {
        Ok(Value::I32(str.len() as i32))
    } else {
        Err(VMError::MismatchedTypes(
            ExprType::String,
            vm.value_type(&args[0]),
        ))
    }
}

pub(crate) fn init_string_methods(host: &mut HostState) {
    host.get_host_type_mut::<String>().unwrap().add_method(
        "len",
        string_len,
        Vec::new(),
        ExprType::I32,
    );
}
