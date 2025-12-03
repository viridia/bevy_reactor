use std::{
    any::TypeId,
    cell::RefCell,
    ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Sub},
};

use bevy::ecs::{component::Component, entity::Entity, world::World};
use bevy_reactor::TrackingScope;
use thiserror::Error;

use crate::{
    compiler,
    decl::DeclKind,
    expr_type::ExprType,
    host::{EntityMember, Global, HostState},
    instr::{self, OP_RET},
};

// Values on the stack
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Value {
    Void,
    Bool(bool),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    Entity(Entity),
}

impl Value {
    pub fn value_type(&self) -> ExprType {
        match self {
            Value::Void => ExprType::Void,
            Value::Bool(_) => ExprType::Boolean,
            Value::I32(_) => ExprType::I32,
            Value::I64(_) => ExprType::I64,
            Value::F32(_) => ExprType::F32,
            Value::F64(_) => ExprType::F64,
            Value::Entity(_) => ExprType::Entity,
        }
    }
}

#[non_exhaustive]
#[derive(Debug, Error, PartialEq)]
pub enum VMError {
    #[error("Invalid instruction")]
    InvalidInstruction,
    #[error("Stack underflow")]
    StackUnderflow,
    #[error("Mismatched types: {0} {1}")]
    MismatchedTypes(ExprType, ExprType),
    #[error("Invalid assignment")]
    InvalidAssignment,
    #[error("Value is not an entity: {0}")]
    NotAnEntity(ExprType),
    #[error("Invalid global index: {0}")]
    InvalidGlobalIndex(usize),
    #[error("Invalid entity prop: {0}")]
    InvalidEntityProp(usize),
    #[error("Invalid entity method: {0}")]
    InvalidEntityMethod(usize),
    #[error("Function not found: {0}")]
    MissingFunction(String),
    #[error("Missing Component: {0:?}")]
    MissingComponent(TypeId),
}

/// A virtual machine for evaluating formulae.
pub struct VM<'w, 'g, 'p> {
    /// Bevy World
    pub world: &'w World,

    /// Entity upon which this script is attached.
    pub owner: Entity,

    /// Globals and entity members
    pub host: &'g HostState,

    /// Execution stack
    stack: Vec<Value>,

    /// Instruction pointer
    iptr: *const u8,

    /// Reactive tracking scope
    pub tracking: RefCell<&'p mut TrackingScope>,
}

type InstrHandler = fn(&mut VM) -> Result<(), VMError>;

impl<'w, 'g, 'p> VM<'w, 'g, 'p> {
    const JUMP_TABLE: [InstrHandler; 256] = build_jump_table();

    // Initialize a new virtual machine.
    pub fn new(world: &'w World, host: &'g HostState, tracking: &'p mut TrackingScope) -> Self {
        Self {
            world,
            owner: Entity::PLACEHOLDER,
            host,
            stack: Vec::new(),
            iptr: Default::default(),
            tracking: RefCell::new(tracking),
        }
    }

    /// Set the entity which owns the script being executed.
    pub fn set_owner(&mut self, owner: Entity) -> &mut Self {
        self.owner = owner;
        self
    }

    /// Align the instruction pointer in preparation for loading an immediate value.
    #[inline(always)]
    fn align_ip<T>(&mut self) {
        let align = std::mem::align_of::<T>();
        let ip_addr = self.iptr as usize;
        let aligned_addr = (ip_addr + (align - 1)) & !(align - 1);
        self.iptr = aligned_addr as *const u8;
    }

    /// Read an immediate value following the current instruction.
    #[inline(always)]
    fn read_immediate<T>(&mut self) -> T {
        self.align_ip::<T>();
        let val = unsafe { std::ptr::read_unaligned(self.iptr as *const T) };
        self.iptr = unsafe { self.iptr.add(std::mem::size_of::<T>()) };
        val
    }

    /// Begin running at the current instruction pointer. Run until we return and the call
    /// stack is empty.
    fn start(&mut self) -> Result<Value, VMError> {
        loop {
            let op = unsafe { *self.iptr };
            // println!("op: {op}");
            if op == OP_RET {
                break;
            }
            self.iptr = unsafe { self.iptr.add(1) };
            VM::JUMP_TABLE[op as usize](self)?;
        }
        Ok(*self.stack.last().unwrap_or(&Value::Void))
    }

    /// Run a compiled function by name.
    pub fn run(
        &mut self,
        module: &compiler::CompiledModule,
        function_name: &str,
    ) -> Result<Value, VMError> {
        if let Some(entry_fn) = module.module_decls.get(function_name) {
            if let DeclKind::Function {
                params: _,
                ret: _,
                is_native: _,
                index,
            } = &entry_fn.kind
            {
                self.iptr = module.functions[*index].code.as_ptr();
            }
            self.start()
        } else {
            Err(VMError::MissingFunction(function_name.to_string()))
        }
    }

    /// Return a reference to the Component `C` on the given entity. Calling this function
    /// adds the component as a dependency of the current tracking scope.
    pub fn component<C: Component>(&self, entity: Entity) -> Option<&C> {
        let comp = self.world.entity(entity).get::<C>();
        self.tracking
            .borrow_mut()
            .track_component::<C>(entity, self.world, comp.is_some());
        comp
    }
}

// Build the jump table
const fn build_jump_table() -> [InstrHandler; 256] {
    let mut table: [InstrHandler; 256] = [invalid; 256];

    table[instr::OP_CONST_TRUE as usize] = const_true;
    table[instr::OP_CONST_FALSE as usize] = const_false;
    table[instr::OP_CONST_I32 as usize] = const_i32;
    table[instr::OP_CONST_I64 as usize] = const_i64;
    table[instr::OP_CONST_F32 as usize] = const_f32;
    table[instr::OP_CONST_F64 as usize] = const_f64;
    table[instr::OP_DROP as usize] = drop1;
    table[instr::OP_DROP_N as usize] = drop_n;
    table[instr::OP_LOAD_PARAM as usize] = load_param;
    table[instr::OP_LOAD_GLOBAL as usize] = load_global;
    table[instr::OP_LOAD_ENTITY_PROP as usize] = load_entity_prop;

    table[instr::OP_LOGICAL_AND as usize] = log_and;
    table[instr::OP_LOGICAL_OR as usize] = log_or;

    table[instr::OP_RET as usize] = ret;
    table[instr::OP_BRANCH as usize] = branch;

    table[instr::OP_CALL_ENTITY_METHOD as usize] = call_entity_method;

    table[instr::OP_ADD_I32 as usize] = add_i32;
    table[instr::OP_ADD_I64 as usize] = add_i64;
    table[instr::OP_ADD_F32 as usize] = add_f32;
    table[instr::OP_ADD_F64 as usize] = add_f64;

    table[instr::OP_SUB_I32 as usize] = sub_i32;
    table[instr::OP_SUB_I64 as usize] = sub_i64;
    table[instr::OP_SUB_F32 as usize] = sub_f32;
    table[instr::OP_SUB_F64 as usize] = sub_f64;

    table[instr::OP_MUL_I32 as usize] = mul_i32;
    table[instr::OP_MUL_I64 as usize] = mul_i64;
    table[instr::OP_MUL_F32 as usize] = mul_f32;
    table[instr::OP_MUL_F64 as usize] = mul_f64;

    table[instr::OP_DIV_I32 as usize] = div_i32;
    table[instr::OP_DIV_I64 as usize] = div_i64;
    table[instr::OP_DIV_F32 as usize] = div_f32;
    table[instr::OP_DIV_F64 as usize] = div_f64;

    table[instr::OP_REM_I32 as usize] = rem_i32;
    table[instr::OP_REM_I64 as usize] = rem_i64;
    table[instr::OP_REM_F32 as usize] = rem_f32;
    table[instr::OP_REM_F64 as usize] = rem_f64;

    table[instr::OP_BIT_AND_I32 as usize] = bit_and_i32;
    table[instr::OP_BIT_AND_I64 as usize] = bit_and_i64;
    table[instr::OP_BIT_OR_I32 as usize] = bit_or_i32;
    table[instr::OP_BIT_OR_I64 as usize] = bit_or_i64;
    table[instr::OP_BIT_XOR_I32 as usize] = bit_xor_i32;
    table[instr::OP_BIT_XOR_I64 as usize] = bit_xor_i64;

    table[instr::OP_EQ_I32 as usize] = eq_i32;
    table[instr::OP_EQ_I64 as usize] = eq_i64;
    table[instr::OP_EQ_F32 as usize] = eq_f32;
    table[instr::OP_EQ_F64 as usize] = eq_f64;

    table[instr::OP_NE_I32 as usize] = ne_i32;
    table[instr::OP_NE_I64 as usize] = ne_i64;
    table[instr::OP_NE_F32 as usize] = ne_f32;
    table[instr::OP_NE_F64 as usize] = ne_f64;

    table[instr::OP_LT_I32 as usize] = lt_i32;
    table[instr::OP_LT_I64 as usize] = lt_i64;
    table[instr::OP_LT_F32 as usize] = lt_f32;
    table[instr::OP_LT_F64 as usize] = lt_f64;

    table[instr::OP_LE_I32 as usize] = le_i32;
    table[instr::OP_LE_I64 as usize] = le_i64;
    table[instr::OP_LE_F32 as usize] = le_f32;
    table[instr::OP_LE_F64 as usize] = le_f64;

    table[instr::OP_GT_I32 as usize] = gt_i32;
    table[instr::OP_GT_I64 as usize] = gt_i64;
    table[instr::OP_GT_F32 as usize] = gt_f32;
    table[instr::OP_GT_F64 as usize] = gt_f64;

    table[instr::OP_GE_I32 as usize] = ge_i32;
    table[instr::OP_GE_I64 as usize] = ge_i64;
    table[instr::OP_GE_F32 as usize] = ge_f32;
    table[instr::OP_GE_F64 as usize] = ge_f64;

    table
}

fn invalid(_vm: &mut VM) -> Result<(), VMError> {
    Err(VMError::InvalidInstruction)
}

fn const_true(vm: &mut VM) -> Result<(), VMError> {
    vm.stack.push(Value::Bool(true));
    // vm.iptr = unsafe { vm.iptr.byte_add(1) };
    Ok(())
}

fn const_false(vm: &mut VM) -> Result<(), VMError> {
    vm.stack.push(Value::Bool(false));
    // vm.iptr = unsafe { vm.iptr.byte_add(1) };
    Ok(())
}

fn const_i32(vm: &mut VM) -> Result<(), VMError> {
    let val = vm.read_immediate::<i32>();
    vm.stack.push(Value::I32(val));
    Ok(())
}

fn const_i64(vm: &mut VM) -> Result<(), VMError> {
    let val = vm.read_immediate::<i64>();
    vm.stack.push(Value::I64(val));
    Ok(())
}

fn const_f32(vm: &mut VM) -> Result<(), VMError> {
    let val = vm.read_immediate::<f32>();
    vm.stack.push(Value::F32(val));
    Ok(())
}

fn const_f64(vm: &mut VM) -> Result<(), VMError> {
    let val = vm.read_immediate::<f64>();
    vm.stack.push(Value::F64(val));
    Ok(())
}

fn drop1(vm: &mut VM) -> Result<(), VMError> {
    vm.stack.pop().ok_or(VMError::StackUnderflow)?;
    Ok(())
}

fn drop_n(vm: &mut VM) -> Result<(), VMError> {
    let n = vm.read_immediate::<u32>() as usize;
    let final_length = vm.stack.len().saturating_sub(n);
    vm.stack.truncate(final_length);
    Ok(())
}

fn load_param(_vm: &mut VM) -> Result<(), VMError> {
    // let n = vm.read_immediate::<u32>() as usize;
    // let final_length = vm.stack.len().saturating_sub(n);
    // vm.stack.truncate(final_length);
    // Ok(())
    todo!();
}

fn load_global(vm: &mut VM) -> Result<(), VMError> {
    let n = vm.read_immediate::<u32>() as usize;
    let val = match vm.host.vars.get(n) {
        Some(Global::Const(val)) => *val,
        Some(Global::Property(accessor)) => accessor(vm)?,
        None => return Err(VMError::InvalidGlobalIndex(n)),
    };
    vm.stack.push(val);
    Ok(())
}

fn load_entity_prop(vm: &mut VM) -> Result<(), VMError> {
    let arg = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
    let Value::Entity(entity) = arg else {
        return Err(VMError::NotAnEntity(arg.value_type()));
    };
    let prop_index = vm.read_immediate::<u32>() as usize;
    let val = match vm.host.entity_members.get(prop_index) {
        Some(EntityMember::Property(accessor)) => accessor(vm, entity)?,
        Some(EntityMember::Method(_)) => return Err(VMError::InvalidEntityProp(prop_index)),
        None => return Err(VMError::InvalidEntityProp(prop_index)),
    };
    vm.stack.push(val);
    Ok(())
}

macro_rules! impl_typed_binop {
    ($name:ident, $variant:ident, $type:ty, $op:ident) => {
        fn $name(vm: &mut VM) -> Result<(), VMError> {
            let rhs = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
            let lhs = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
            let val = if let (Value::$variant(l), Value::$variant(r)) = (lhs, rhs) {
                Value::$variant(l.$op(r))
            } else {
                return Err(VMError::MismatchedTypes(lhs.value_type(), rhs.value_type()));
            };
            vm.stack.push(val);
            Ok(())
        }
    };
}

macro_rules! impl_relational_binop {
    ($name:ident, $variant:ident, $type:ty, $op:ident) => {
        fn $name(vm: &mut VM) -> Result<(), VMError> {
            let rhs = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
            let lhs = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
            let val = if let (Value::$variant(l), Value::$variant(r)) = (lhs, rhs) {
                Value::Bool(l.$op(&r))
            } else {
                return Err(VMError::MismatchedTypes(lhs.value_type(), rhs.value_type()));
            };
            vm.stack.push(val);
            Ok(())
        }
    };
}

impl_typed_binop!(add_i32, I32, i32, add);
impl_typed_binop!(add_i64, I64, i64, add);
impl_typed_binop!(add_f32, F32, f32, add);
impl_typed_binop!(add_f64, F64, f64, add);

impl_typed_binop!(sub_i32, I32, i32, sub);
impl_typed_binop!(sub_i64, I64, i64, sub);
impl_typed_binop!(sub_f32, F32, f32, sub);
impl_typed_binop!(sub_f64, F64, f64, sub);

impl_typed_binop!(mul_i32, I32, i32, mul);
impl_typed_binop!(mul_i64, I64, i64, mul);
impl_typed_binop!(mul_f32, F32, f32, mul);
impl_typed_binop!(mul_f64, F64, f64, mul);

impl_typed_binop!(div_i32, I32, i32, div);
impl_typed_binop!(div_i64, I64, i64, div);
impl_typed_binop!(div_f32, F32, f32, div);
impl_typed_binop!(div_f64, F64, f64, div);

impl_typed_binop!(rem_i32, I32, i32, rem);
impl_typed_binop!(rem_i64, I64, i64, rem);
impl_typed_binop!(rem_f32, F32, f32, rem);
impl_typed_binop!(rem_f64, F64, f64, rem);

impl_typed_binop!(bit_and_i32, I32, i32, bitand);
impl_typed_binop!(bit_and_i64, I64, i64, bitand);

impl_typed_binop!(bit_or_i32, I32, i32, bitor);
impl_typed_binop!(bit_or_i64, I64, i64, bitor);

impl_typed_binop!(bit_xor_i32, I32, i32, bitxor);
impl_typed_binop!(bit_xor_i64, I64, i64, bitxor);

impl_relational_binop!(eq_i32, I32, i32, eq);
impl_relational_binop!(eq_i64, I64, i64, eq);
impl_relational_binop!(eq_f32, F32, f32, eq);
impl_relational_binop!(eq_f64, F64, f64, eq);

impl_relational_binop!(ne_i32, I32, i32, ne);
impl_relational_binop!(ne_i64, I64, i64, ne);
impl_relational_binop!(ne_f32, F32, f32, ne);
impl_relational_binop!(ne_f64, F64, f64, ne);

impl_relational_binop!(lt_i32, I32, i32, lt);
impl_relational_binop!(lt_i64, I64, i64, lt);
impl_relational_binop!(lt_f32, F32, f32, lt);
impl_relational_binop!(lt_f64, F64, f64, lt);

impl_relational_binop!(le_i32, I32, i32, le);
impl_relational_binop!(le_i64, I64, i64, le);
impl_relational_binop!(le_f32, F32, f32, le);
impl_relational_binop!(le_f64, F64, f64, le);

impl_relational_binop!(gt_i32, I32, i32, gt);
impl_relational_binop!(gt_i64, I64, i64, gt);
impl_relational_binop!(gt_f32, F32, f32, gt);
impl_relational_binop!(gt_f64, F64, f64, gt);

impl_relational_binop!(ge_i32, I32, i32, ge);
impl_relational_binop!(ge_i64, I64, i64, ge);
impl_relational_binop!(ge_f32, F32, f32, ge);
impl_relational_binop!(ge_f64, F64, f64, ge);

fn log_and(vm: &mut VM) -> Result<(), VMError> {
    let rhs = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
    let lhs = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
    let val = match (lhs, rhs) {
        (Value::Bool(l), Value::Bool(r)) => Value::Bool(l && r),
        (v0, v1) => {
            return Err(VMError::MismatchedTypes(v0.value_type(), v1.value_type()));
        }
    };
    vm.stack.push(val);
    Ok(())
}

fn log_or(vm: &mut VM) -> Result<(), VMError> {
    let rhs = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
    let lhs = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
    let val = match (lhs, rhs) {
        (Value::Bool(l), Value::Bool(r)) => Value::Bool(l && r),
        (v0, v1) => {
            return Err(VMError::MismatchedTypes(v0.value_type(), v1.value_type()));
        }
    };
    vm.stack.push(val);
    Ok(())
}

// TODO:
// pub const OP_SHL: u8 = 40;
// pub const OP_SHR: u8 = 41;

// // Unops: all consume TOS and push result
// pub const OP_LOG_NOT: u8 = 50;
// pub const OP_NEGATE: u8 = 51;
// pub const OP_COMPLEMENT: u8 = 52;
// pub const OP_CALL: u8 = 60; // (imm i32 num arguments, consumes TOS + num arguments)

fn branch(vm: &mut VM) -> Result<(), VMError> {
    let offset = vm.read_immediate::<i32>();
    vm.iptr = unsafe { vm.iptr.offset(offset as isize) };
    Ok(())
}

fn ret(_vm: &mut VM) -> Result<(), VMError> {
    // `ret` is handled by the interpreter loop.
    unreachable!("`ret` instruction handler should not be called");
}

fn call_entity_method(vm: &mut VM) -> Result<(), VMError> {
    let arg = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
    let Value::Entity(entity) = arg else {
        return Err(VMError::NotAnEntity(arg.value_type()));
    };
    let num_args = vm.read_immediate::<u32>() as usize;
    let method_index = vm.read_immediate::<u32>() as usize;
    let stack_len = vm.stack.len();
    if num_args > stack_len {
        // return Err(VMError::NotAnEntity(arg.value_type()));
    }
    let args = &vm.stack[stack_len - num_args..stack_len];
    let val = match vm.host.entity_members.get(method_index) {
        Some(EntityMember::Method(method)) => method(vm, entity, args)?,
        Some(EntityMember::Property(_accessor)) => {
            return Err(VMError::InvalidEntityMethod(method_index))?;
        }
        None => return Err(VMError::InvalidEntityMethod(method_index)),
    };
    // Drop args
    vm.stack.truncate(stack_len - num_args);
    vm.stack.push(val);
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::any::Any;

    use bevy::ecs::component::{Component, Tick};

    use crate::{expr_type::ExprType, instr};

    use super::*;

    #[test]
    fn test_invalid_instr() {
        let world = World::new();
        let host = HostState::default();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut builder = instr::InstructionBuilder::default();
        builder.push_op(255);
        let code = builder.inner();
        let mut vm = VM::new(&world, &host, &mut tracking);
        vm.iptr = code.as_ptr();
        let error = vm.start().unwrap_err();
        assert_eq!(error, VMError::InvalidInstruction);
    }

    #[test]
    fn test_const_bool() {
        let world = World::new();
        let host = HostState::default();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut builder = instr::InstructionBuilder::default();
        builder.push_op(instr::OP_CONST_TRUE);
        builder.push_op(instr::OP_RET);
        let code = builder.inner();
        let mut vm = VM::new(&world, &host, &mut tracking);
        vm.iptr = code.as_ptr();
        let result = vm.start().unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn test_const_i32() {
        let world = World::new();
        let host = HostState::default();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut builder = instr::InstructionBuilder::default();
        builder.push_op(instr::OP_CONST_I32);
        builder.push_immediate::<i32>(3);
        builder.push_op(instr::OP_RET);
        let code = builder.inner();
        let mut vm = VM::new(&world, &host, &mut tracking);
        vm.iptr = code.as_ptr();
        let result = vm.start().unwrap();
        assert_eq!(result, Value::I32(3));
    }

    #[test]
    fn test_add_i32() {
        let world = World::new();
        let host = HostState::default();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut builder = instr::InstructionBuilder::default();
        builder.push_op(instr::OP_CONST_I32);
        builder.push_immediate::<i32>(5);
        builder.push_op(instr::OP_CONST_I32);
        builder.push_immediate::<i32>(1);
        builder.push_op(instr::OP_ADD_I32);
        builder.push_op(instr::OP_RET);
        let code = builder.inner();
        let mut vm = VM::new(&world, &host, &mut tracking);
        vm.iptr = code.as_ptr();
        let result = vm.start().unwrap();
        assert_eq!(result, Value::I32(6));
    }

    #[test]
    fn test_add_f32() {
        let world = World::new();
        let host = HostState::default();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut builder = instr::InstructionBuilder::default();
        builder.push_op(instr::OP_CONST_F32);
        builder.push_immediate::<f32>(5.0);
        builder.push_op(instr::OP_CONST_F32);
        builder.push_immediate::<f32>(1.0);
        builder.push_op(instr::OP_ADD_F32);
        builder.push_op(instr::OP_RET);
        let code = builder.inner();
        let mut vm = VM::new(&world, &host, &mut tracking);
        vm.iptr = code.as_ptr();
        let result = vm.start().unwrap();
        assert_eq!(result, Value::F32(6.0));
    }

    #[test]
    fn test_add_f64() {
        let world = World::new();
        let host = HostState::default();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut builder = instr::InstructionBuilder::default();
        builder.push_op(instr::OP_CONST_F64);
        builder.push_immediate::<f64>(5.0);
        builder.push_op(instr::OP_CONST_F64);
        builder.push_immediate::<f64>(1.0);
        builder.push_op(instr::OP_ADD_F64);
        builder.push_op(instr::OP_RET);
        let code = builder.inner();
        let mut vm = VM::new(&world, &host, &mut tracking);
        vm.iptr = code.as_ptr();
        let result = vm.start().unwrap();
        assert_eq!(result, Value::F64(6.0));
    }

    #[test]
    fn test_add_mismatched() {
        let world = World::new();
        let host = HostState::default();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut builder = instr::InstructionBuilder::default();
        builder.push_op(instr::OP_CONST_I32);
        builder.push_immediate::<i32>(5);
        builder.push_op(instr::OP_CONST_F32);
        builder.push_immediate::<f32>(1.0);
        builder.push_op(instr::OP_ADD_I32);
        builder.push_op(instr::OP_RET);
        let code = builder.inner();
        let mut vm = VM::new(&world, &host, &mut tracking);
        vm.iptr = code.as_ptr();
        let error = vm.start().unwrap_err();
        assert_eq!(
            error,
            VMError::MismatchedTypes(ExprType::I32, ExprType::F32)
        );
    }

    #[test]
    fn test_sub_i32() {
        let world = World::new();
        let host = HostState::default();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut builder = instr::InstructionBuilder::default();
        builder.push_op(instr::OP_CONST_I32);
        builder.push_immediate::<i32>(5);
        builder.push_op(instr::OP_CONST_I32);
        builder.push_immediate::<i32>(1);
        builder.push_op(instr::OP_SUB_I32);
        builder.push_op(instr::OP_RET);
        let code = builder.inner();
        let mut vm = VM::new(&world, &host, &mut tracking);
        vm.iptr = code.as_ptr();
        let result = vm.start().unwrap();
        assert_eq!(result, Value::I32(4));
    }

    #[test]
    fn test_bit_and_i32() {
        let world = World::new();
        let host = HostState::default();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut builder = instr::InstructionBuilder::default();
        builder.push_op(instr::OP_CONST_I32);
        builder.push_immediate::<i32>(5);
        builder.push_op(instr::OP_CONST_I32);
        builder.push_immediate::<i32>(1);
        builder.push_op(instr::OP_BIT_AND_I32);
        builder.push_op(instr::OP_RET);
        let code = builder.inner();
        let mut vm = VM::new(&world, &host, &mut tracking);
        vm.iptr = code.as_ptr();
        let result = vm.start().unwrap();
        assert_eq!(result, Value::I32(1));
    }

    #[test]
    fn test_bit_or_i32() {
        let world = World::new();
        let host = HostState::default();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut builder = instr::InstructionBuilder::default();
        builder.push_op(instr::OP_CONST_I32);
        builder.push_immediate::<i32>(4);
        builder.push_op(instr::OP_CONST_I32);
        builder.push_immediate::<i32>(1);
        builder.push_op(instr::OP_BIT_OR_I32);
        builder.push_op(instr::OP_RET);
        let code = builder.inner();
        let mut vm = VM::new(&world, &host, &mut tracking);
        vm.iptr = code.as_ptr();
        let result = vm.start().unwrap();
        assert_eq!(result, Value::I32(5));
    }

    // TODO:
    // - branch
    // - drop
    // - drop_n

    #[derive(Component)]
    struct Health(f32);

    #[test]
    fn test_global() {
        let mut world = World::new();
        let actor = world.spawn(Health(22.0));
        let actor_id = actor.id();
        let mut host = HostState::default();
        let self_id = host.add_global_prop("self", get_self, ExprType::Entity);
        let health_id = host.add_entity_prop("health", entity_health, ExprType::F32);
        let mut tracking = TrackingScope::new(Tick::default());
        let mut builder = instr::InstructionBuilder::default();
        builder.push_op(instr::OP_LOAD_GLOBAL);
        builder.push_immediate::<u32>(self_id as u32);
        builder.push_op(instr::OP_LOAD_ENTITY_PROP);
        builder.push_immediate::<u32>(health_id as u32);
        builder.push_op(instr::OP_RET);
        let code = builder.inner();
        let mut vm = VM::new(&world, &host, &mut tracking);
        vm.owner = actor_id;
        vm.iptr = code.as_ptr();
        let result = vm.start().unwrap();
        assert_eq!(result, Value::F32(22.0));
    }

    fn get_self(vm: &VM) -> Result<Value, VMError> {
        Ok(Value::Entity(vm.owner))
    }

    fn entity_health(vm: &VM, actor: Entity) -> Result<Value, VMError> {
        if let Some(&Health(h)) = vm.component::<Health>(actor) {
            Ok(Value::F32(h))
        } else {
            Err(VMError::MissingComponent(Health.type_id()))
        }
    }
}
