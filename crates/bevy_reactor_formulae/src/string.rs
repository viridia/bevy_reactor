//! String type and methods.

// use std::sync::OnceLock;

use std::sync::Arc;

use crate::{
    HostState, Value,
    expr_type::ExprType,
    vm::{InvocationContext, VMError},
};

// static STRING_METHODS: OnceLock<ObjectMembers> = OnceLock::new();

// pub(crate) fn get_string_methods() -> &'static ObjectMembers {
//     STRING_METHODS.get_or_init(|| {
//         let mut table = ObjectMembers::default();
//         table.add_method("len", string_len, Vec::new(), ExprType::I32);
//         table
//     })
// }

/// string.len()
fn string_len(ctx: &mut InvocationContext) -> Result<Value, VMError> {
    assert_eq!(ctx.num_arguments(), 1);
    let s: Arc<String> = ctx.argument(0)?;
    Ok(Value::I32(s.len() as i32))
}

pub(crate) fn init_string_methods(host: &mut HostState) {
    host.get_host_type_mut::<String>().unwrap().add_method(
        "len",
        string_len,
        Vec::new(),
        ExprType::I32,
    );
}
