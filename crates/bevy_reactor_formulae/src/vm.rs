use std::{
    any::TypeId,
    cell::RefCell,
    ops::{Add, Div, Mul, Rem, Sub},
};

use bevy::ecs::{entity::Entity, world::World};
use bevy_reactor::TrackingScope;
use thiserror::Error;

use crate::{
    instr::{self, OP_RET},
    symbol_table::SymbolTable,
};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ValueType {
    Void,
    Bool,
    I32,
    I64,
    F32,
    F64,
    Symbol,
    Entity,
}

impl std::fmt::Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            ValueType::Void => "void",
            ValueType::Bool => "bool",
            ValueType::I32 => "i32",
            ValueType::I64 => "i64",
            ValueType::F32 => "f32",
            ValueType::F64 => "f64",
            ValueType::Symbol => "Symbol",
            ValueType::Entity => "Entity",
        };
        f.write_str(s)
    }
}

// Values on the stack
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Value {
    Void,
    Bool(bool),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    Symbol(&'static str),
    Entity(Entity),
}

impl Value {
    pub fn value_type(&self) -> ValueType {
        match self {
            Value::Void => ValueType::Void,
            Value::Bool(_) => ValueType::Bool,
            Value::I32(_) => ValueType::I32,
            Value::I64(_) => ValueType::I64,
            Value::F32(_) => ValueType::F32,
            Value::F64(_) => ValueType::F64,
            Value::Symbol(_) => ValueType::Symbol,
            Value::Entity(_) => ValueType::Entity,
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
    MismatchedTypes(ValueType, ValueType),
    #[error("Invalid assignment")]
    InvalidAssignment,
    #[error("Value is not an entity: {0}")]
    NotAnEntity(ValueType),
    #[error("Invalid global index: {0}")]
    InvalidGlobalIndex(usize),
    #[error("Invalid entity prop: {0}")]
    InvalidEntityProp(usize),
    #[error("Invalid entity method: {0}")]
    InvalidEntityMethod(usize),
    #[error("Missing Component: {0:?}")]
    MissingComponent(TypeId),
}

type GlobalPropHandler = fn(&VM) -> Result<Value, VMError>;

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Global {
    /// A constant value.
    Const(Value),

    /// A dynamic property, such as `time`.
    Property(GlobalPropHandler),
}

type EntityProperty = fn(&VM, e: Entity) -> Result<Value, VMError>;
type EntityMethod = fn(&VM, e: Entity, args: &[Value]) -> Result<Value, VMError>;

/// A virtual machine for evaluating formulae.
pub struct VM<'w, 'g, 'p> {
    /// Bevy World
    pub world: &'w World,

    /// Entity upon which this script is attached.
    pub owner: Entity,

    /// Global variables
    globals: &'g SymbolTable<Global>,

    /// Properties of entities
    entity_props: &'g SymbolTable<EntityProperty>,

    /// Methods of entities
    entity_methods: &'g SymbolTable<EntityMethod>,

    /// Execution stack
    stack: Vec<Value>,

    /// Instruction pointer
    iptr: *const u8,

    /// Reactive tracking scope
    pub tracking: RefCell<&'p mut TrackingScope>,
}

type InstrHandler = unsafe fn(&mut VM) -> Result<(), VMError>;

impl<'w, 'g, 'p> VM<'w, 'g, 'p> {
    const JUMP_TABLE: [InstrHandler; 256] = build_jump_table();

    /// Initialize a new virtual machine.
    // pub fn new(world: &'w World, globals: &'g Globals, tracking: &'p mut TrackingScope) -> Self {
    //     Self {
    //         world,
    //         owner: Entity::PLACEHOLDER,
    //         globals,
    //         stack: Vec::new(),
    //         iptr: Default::default(),
    //         tracking,
    //     }
    // }

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

    /// Run a script.
    pub fn run(&mut self) -> Result<Value, VMError> {
        loop {
            let op = unsafe { *self.iptr };
            // println!("op: {op}");
            if op == OP_RET {
                break;
            }
            self.iptr = unsafe { self.iptr.add(1) };
            unsafe { VM::JUMP_TABLE[op as usize](self)? };
        }
        Ok(*self.stack.last().unwrap_or(&Value::Void))
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

    table[instr::OP_ADD as usize] = add;
    table[instr::OP_SUB as usize] = sub;
    table[instr::OP_MUL as usize] = mul;
    table[instr::OP_DIV as usize] = div;
    table[instr::OP_REM as usize] = rem;
    table[instr::OP_LOGICAL_AND as usize] = log_and;
    table[instr::OP_LOGICAL_OR as usize] = log_or;
    table[instr::OP_BIT_AND as usize] = bit_and;
    table[instr::OP_BIT_OR as usize] = bit_or;
    table[instr::OP_BIT_XOR as usize] = bit_xor;

    table[instr::OP_RET as usize] = ret;
    table[instr::OP_BRANCH as usize] = branch;

    table[instr::OP_CALL_ENTITY_METHOD as usize] = call_entity_method;

    table
}

unsafe fn invalid(_vm: &mut VM) -> Result<(), VMError> {
    Err(VMError::InvalidInstruction)
}

unsafe fn const_true(vm: &mut VM) -> Result<(), VMError> {
    vm.stack.push(Value::Bool(true));
    // vm.iptr = unsafe { vm.iptr.byte_add(1) };
    Ok(())
}

unsafe fn const_false(vm: &mut VM) -> Result<(), VMError> {
    vm.stack.push(Value::Bool(false));
    // vm.iptr = unsafe { vm.iptr.byte_add(1) };
    Ok(())
}

unsafe fn const_i32(vm: &mut VM) -> Result<(), VMError> {
    let val = vm.read_immediate::<i32>();
    vm.stack.push(Value::I32(val));
    Ok(())
}

unsafe fn const_i64(vm: &mut VM) -> Result<(), VMError> {
    let val = vm.read_immediate::<i64>();
    vm.stack.push(Value::I64(val));
    Ok(())
}

unsafe fn const_f32(vm: &mut VM) -> Result<(), VMError> {
    let val = vm.read_immediate::<f32>();
    vm.stack.push(Value::F32(val));
    Ok(())
}

unsafe fn const_f64(vm: &mut VM) -> Result<(), VMError> {
    let val = vm.read_immediate::<f64>();
    vm.stack.push(Value::F64(val));
    Ok(())
}

unsafe fn drop1(vm: &mut VM) -> Result<(), VMError> {
    vm.stack.pop().ok_or(VMError::StackUnderflow)?;
    Ok(())
}

unsafe fn drop_n(vm: &mut VM) -> Result<(), VMError> {
    let n = vm.read_immediate::<u32>() as usize;
    let final_length = vm.stack.len().saturating_sub(n);
    vm.stack.truncate(final_length);
    Ok(())
}

unsafe fn load_param(_vm: &mut VM) -> Result<(), VMError> {
    // let n = vm.read_immediate::<u32>() as usize;
    // let final_length = vm.stack.len().saturating_sub(n);
    // vm.stack.truncate(final_length);
    // Ok(())
    todo!();
}

unsafe fn load_global(vm: &mut VM) -> Result<(), VMError> {
    let n = vm.read_immediate::<u32>() as usize;
    let val = match vm.globals.get(n) {
        Some(Global::Const(val)) => *val,
        Some(Global::Property(prop)) => prop(vm)?,
        None => return Err(VMError::InvalidGlobalIndex(n)),
    };
    vm.stack.push(val);
    Ok(())
}

unsafe fn load_entity_prop(vm: &mut VM) -> Result<(), VMError> {
    let arg = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
    let Value::Entity(entity) = arg else {
        return Err(VMError::NotAnEntity(arg.value_type()));
    };
    let prop_index = vm.read_immediate::<u32>() as usize;
    let val = match vm.entity_props.get(prop_index) {
        Some(prop) => prop(vm, entity)?,
        None => return Err(VMError::InvalidEntityProp(prop_index)),
    };
    vm.stack.push(val);
    Ok(())
}

unsafe fn add(vm: &mut VM) -> Result<(), VMError> {
    let rhs = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
    let lhs = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
    let val = match (lhs, rhs) {
        (Value::I32(l), Value::I32(r)) => Value::I32(l.add(r)),
        (Value::I64(l), Value::I64(r)) => Value::I64(l.add(r)),
        (Value::F32(l), Value::F32(r)) => Value::F32(l.add(r)),
        (Value::F64(l), Value::F64(r)) => Value::F64(l.add(r)),
        (v0, v1) => {
            return Err(VMError::MismatchedTypes(v0.value_type(), v1.value_type()));
        }
    };
    vm.stack.push(val);
    Ok(())
}

unsafe fn sub(vm: &mut VM) -> Result<(), VMError> {
    let rhs = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
    let lhs = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
    let val = match (lhs, rhs) {
        (Value::I32(l), Value::I32(r)) => Value::I32(l.sub(r)),
        (Value::I64(l), Value::I64(r)) => Value::I64(l.sub(r)),
        (Value::F32(l), Value::F32(r)) => Value::F32(l.sub(r)),
        (Value::F64(l), Value::F64(r)) => Value::F64(l.sub(r)),
        (v0, v1) => {
            return Err(VMError::MismatchedTypes(v0.value_type(), v1.value_type()));
        }
    };
    vm.stack.push(val);
    Ok(())
}

unsafe fn mul(vm: &mut VM) -> Result<(), VMError> {
    let rhs = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
    let lhs = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
    let val = match (lhs, rhs) {
        (Value::I32(l), Value::I32(r)) => Value::I32(l.mul(r)),
        (Value::I64(l), Value::I64(r)) => Value::I64(l.mul(r)),
        (Value::F32(l), Value::F32(r)) => Value::F32(l.mul(r)),
        (Value::F64(l), Value::F64(r)) => Value::F64(l.mul(r)),
        (v0, v1) => {
            return Err(VMError::MismatchedTypes(v0.value_type(), v1.value_type()));
        }
    };
    vm.stack.push(val);
    Ok(())
}

unsafe fn div(vm: &mut VM) -> Result<(), VMError> {
    let rhs = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
    let lhs = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
    let val = match (lhs, rhs) {
        (Value::I32(l), Value::I32(r)) => Value::I32(l.div(r)),
        (Value::I64(l), Value::I64(r)) => Value::I64(l.div(r)),
        (Value::F32(l), Value::F32(r)) => Value::F32(l.div(r)),
        (Value::F64(l), Value::F64(r)) => Value::F64(l.div(r)),
        (v0, v1) => {
            return Err(VMError::MismatchedTypes(v0.value_type(), v1.value_type()));
        }
    };
    vm.stack.push(val);
    Ok(())
}

unsafe fn rem(vm: &mut VM) -> Result<(), VMError> {
    let rhs = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
    let lhs = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
    let val = match (lhs, rhs) {
        (Value::I32(l), Value::I32(r)) => Value::I32(l.rem(r)),
        (Value::I64(l), Value::I64(r)) => Value::I64(l.rem(r)),
        (Value::F32(l), Value::F32(r)) => Value::F32(l.rem(r)),
        (Value::F64(l), Value::F64(r)) => Value::F64(l.rem(r)),
        (v0, v1) => {
            return Err(VMError::MismatchedTypes(v0.value_type(), v1.value_type()));
        }
    };
    vm.stack.push(val);
    Ok(())
}

unsafe fn log_and(vm: &mut VM) -> Result<(), VMError> {
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

unsafe fn log_or(vm: &mut VM) -> Result<(), VMError> {
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

unsafe fn bit_and(vm: &mut VM) -> Result<(), VMError> {
    let rhs = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
    let lhs = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
    let val = match (lhs, rhs) {
        (Value::Bool(l), Value::Bool(r)) => Value::Bool(l & r),
        (Value::I32(l), Value::I32(r)) => Value::I32(l & r),
        (Value::I64(l), Value::I64(r)) => Value::I64(l & r),
        (v0, v1) => {
            return Err(VMError::MismatchedTypes(v0.value_type(), v1.value_type()));
        }
    };
    vm.stack.push(val);
    Ok(())
}

unsafe fn bit_or(vm: &mut VM) -> Result<(), VMError> {
    let rhs = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
    let lhs = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
    let val = match (lhs, rhs) {
        (Value::Bool(l), Value::Bool(r)) => Value::Bool(l | r),
        (Value::I32(l), Value::I32(r)) => Value::I32(l | r),
        (Value::I64(l), Value::I64(r)) => Value::I64(l | r),
        (v0, v1) => {
            return Err(VMError::MismatchedTypes(v0.value_type(), v1.value_type()));
        }
    };
    vm.stack.push(val);
    Ok(())
}

unsafe fn bit_xor(vm: &mut VM) -> Result<(), VMError> {
    let rhs = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
    let lhs = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
    let val = match (lhs, rhs) {
        (Value::Bool(l), Value::Bool(r)) => Value::Bool(l ^ r),
        (Value::I32(l), Value::I32(r)) => Value::I32(l ^ r),
        (Value::I64(l), Value::I64(r)) => Value::I64(l ^ r),
        (v0, v1) => {
            return Err(VMError::MismatchedTypes(v0.value_type(), v1.value_type()));
        }
    };
    vm.stack.push(val);
    Ok(())
}

// pub const OP_SHL: u8 = 40;
// pub const OP_SHR: u8 = 41;
// pub const OP_EQUAL: u8 = 42;
// pub const OP_NOT_EQUAL: u8 = 43;
// pub const OP_LESS: u8 = 44;
// pub const OP_LESS_EQUAL: u8 = 45;
// pub const OP_GREATER: u8 = 46;
// pub const OP_GREATER_EQUAL: u8 = 47;

// // Unops: all consume TOS and push result
// pub const OP_LOG_NOT: u8 = 50;
// pub const OP_NEGATE: u8 = 51;
// pub const OP_COMPLEMENT: u8 = 52;
// pub const OP_CALL: u8 = 60; // (imm i32 num arguments, consumes TOS + num arguments)

unsafe fn branch(vm: &mut VM) -> Result<(), VMError> {
    let offset = vm.read_immediate::<i32>();
    vm.iptr = unsafe { vm.iptr.offset(offset as isize) };
    Ok(())
}

unsafe fn ret(_vm: &mut VM) -> Result<(), VMError> {
    // `ret` is handled by the interpreter loop.
    unreachable!("`ret` instruction handler should not be called");
}

unsafe fn call_entity_method(vm: &mut VM) -> Result<(), VMError> {
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
    let val = match vm.entity_methods.get(method_index) {
        Some(method) => method(vm, entity, args)?,
        None => return Err(VMError::InvalidGlobalIndex(method_index)),
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

    use crate::instr;

    use super::*;

    #[test]
    fn test_invalid_instr() {
        let world = World::new();
        let globals = SymbolTable::<Global>::empty();
        let entity_props = SymbolTable::<EntityProperty>::empty();
        let entity_methods = SymbolTable::<EntityMethod>::empty();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut builder = instr::InstructionBuilder::default();
        builder.push_op(255);
        let code = builder.inner();
        let mut vm = VM {
            world: &world,
            owner: Entity::PLACEHOLDER,
            globals: &globals,
            entity_props: &entity_props,
            entity_methods: &entity_methods,
            stack: Vec::with_capacity(100),
            iptr: code.as_ptr(),
            tracking: RefCell::new(&mut tracking),
        };
        let error = vm.run().unwrap_err();
        assert_eq!(error, VMError::InvalidInstruction);
    }

    #[test]
    fn test_const_bool() {
        let world = World::new();
        let globals = SymbolTable::<Global>::empty();
        let entity_props = SymbolTable::<EntityProperty>::empty();
        let entity_methods = SymbolTable::<EntityMethod>::empty();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut builder = instr::InstructionBuilder::default();
        builder.push_op(instr::OP_CONST_TRUE);
        builder.push_op(instr::OP_RET);
        let code = builder.inner();
        let mut vm = VM {
            world: &world,
            owner: Entity::PLACEHOLDER,
            globals: &globals,
            entity_props: &entity_props,
            entity_methods: &entity_methods,
            stack: Vec::with_capacity(100),
            iptr: code.as_ptr(),
            tracking: RefCell::new(&mut tracking),
        };
        let result = vm.run().unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn test_const_i32() {
        let world = World::new();
        let globals = SymbolTable::<Global>::empty();
        let entity_props = SymbolTable::<EntityProperty>::empty();
        let entity_methods = SymbolTable::<EntityMethod>::empty();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut builder = instr::InstructionBuilder::default();
        builder.push_op(instr::OP_CONST_I32);
        builder.push_immediate::<i32>(3);
        builder.push_op(instr::OP_RET);
        let code = builder.inner();
        let mut vm = VM {
            world: &world,
            owner: Entity::PLACEHOLDER,
            globals: &globals,
            entity_props: &entity_props,
            entity_methods: &entity_methods,
            stack: Vec::with_capacity(100),
            iptr: code.as_ptr(),
            tracking: RefCell::new(&mut tracking),
        };
        let result = vm.run().unwrap();
        assert_eq!(result, Value::I32(3));
    }

    #[test]
    fn test_add_i32() {
        let world = World::new();
        let globals = SymbolTable::<Global>::empty();
        let entity_props = SymbolTable::<EntityProperty>::empty();
        let entity_methods = SymbolTable::<EntityMethod>::empty();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut builder = instr::InstructionBuilder::default();
        builder.push_op(instr::OP_CONST_I32);
        builder.push_immediate::<i32>(5);
        builder.push_op(instr::OP_CONST_I32);
        builder.push_immediate::<i32>(1);
        builder.push_op(instr::OP_ADD);
        builder.push_op(instr::OP_RET);
        let code = builder.inner();
        let mut vm = VM {
            world: &world,
            owner: Entity::PLACEHOLDER,
            globals: &globals,
            entity_props: &entity_props,
            entity_methods: &entity_methods,
            stack: Vec::with_capacity(100),
            iptr: code.as_ptr(),
            tracking: RefCell::new(&mut tracking),
        };
        let result = vm.run().unwrap();
        assert_eq!(result, Value::I32(6));
    }

    #[test]
    fn test_add_f32() {
        let world = World::new();
        let globals = SymbolTable::<Global>::empty();
        let entity_props = SymbolTable::<EntityProperty>::empty();
        let entity_methods = SymbolTable::<EntityMethod>::empty();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut builder = instr::InstructionBuilder::default();
        builder.push_op(instr::OP_CONST_F32);
        builder.push_immediate::<f32>(5.0);
        builder.push_op(instr::OP_CONST_F32);
        builder.push_immediate::<f32>(1.0);
        builder.push_op(instr::OP_ADD);
        builder.push_op(instr::OP_RET);
        let code = builder.inner();
        let mut vm = VM {
            world: &world,
            owner: Entity::PLACEHOLDER,
            globals: &globals,
            entity_props: &entity_props,
            entity_methods: &entity_methods,
            stack: Vec::with_capacity(100),
            iptr: code.as_ptr(),
            tracking: RefCell::new(&mut tracking),
        };
        let result = vm.run().unwrap();
        assert_eq!(result, Value::F32(6.0));
    }

    #[test]
    fn test_add_f64() {
        let world = World::new();
        let globals = SymbolTable::<Global>::empty();
        let entity_props = SymbolTable::<EntityProperty>::empty();
        let entity_methods = SymbolTable::<EntityMethod>::empty();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut builder = instr::InstructionBuilder::default();
        builder.push_op(instr::OP_CONST_F64);
        builder.push_immediate::<f64>(5.0);
        builder.push_op(instr::OP_CONST_F64);
        builder.push_immediate::<f64>(1.0);
        builder.push_op(instr::OP_ADD);
        builder.push_op(instr::OP_RET);
        let code = builder.inner();
        let mut vm = VM {
            world: &world,
            owner: Entity::PLACEHOLDER,
            globals: &globals,
            entity_props: &entity_props,
            entity_methods: &entity_methods,
            stack: Vec::with_capacity(100),
            iptr: code.as_ptr(),
            tracking: RefCell::new(&mut tracking),
        };
        let result = vm.run().unwrap();
        assert_eq!(result, Value::F64(6.0));
    }

    #[test]
    fn test_add_mismatched() {
        let world = World::new();
        let globals = SymbolTable::<Global>::empty();
        let entity_props = SymbolTable::<EntityProperty>::empty();
        let entity_methods = SymbolTable::<EntityMethod>::empty();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut builder = instr::InstructionBuilder::default();
        builder.push_op(instr::OP_CONST_I32);
        builder.push_immediate::<i32>(5);
        builder.push_op(instr::OP_CONST_F32);
        builder.push_immediate::<f32>(1.0);
        builder.push_op(instr::OP_ADD);
        builder.push_op(instr::OP_RET);
        let code = builder.inner();
        let mut vm = VM {
            world: &world,
            owner: Entity::PLACEHOLDER,
            globals: &globals,
            entity_props: &entity_props,
            entity_methods: &entity_methods,
            stack: Vec::with_capacity(100),
            iptr: code.as_ptr(),
            tracking: RefCell::new(&mut tracking),
        };
        let error = vm.run().unwrap_err();
        assert_eq!(
            error,
            VMError::MismatchedTypes(ValueType::I32, ValueType::F32)
        );
    }

    #[test]
    fn test_sub_i32() {
        let world = World::new();
        let globals = SymbolTable::<Global>::empty();
        let entity_props = SymbolTable::<EntityProperty>::empty();
        let entity_methods = SymbolTable::<EntityMethod>::empty();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut builder = instr::InstructionBuilder::default();
        builder.push_op(instr::OP_CONST_I32);
        builder.push_immediate::<i32>(5);
        builder.push_op(instr::OP_CONST_I32);
        builder.push_immediate::<i32>(1);
        builder.push_op(instr::OP_SUB);
        builder.push_op(instr::OP_RET);
        let code = builder.inner();
        let mut vm = VM {
            world: &world,
            owner: Entity::PLACEHOLDER,
            globals: &globals,
            entity_props: &entity_props,
            entity_methods: &entity_methods,
            stack: Vec::with_capacity(100),
            iptr: code.as_ptr(),
            tracking: RefCell::new(&mut tracking),
        };
        let result = vm.run().unwrap();
        assert_eq!(result, Value::I32(4));
    }

    #[test]
    fn test_bit_and_i32() {
        let world = World::new();
        let globals = SymbolTable::<Global>::empty();
        let entity_props = SymbolTable::<EntityProperty>::empty();
        let entity_methods = SymbolTable::<EntityMethod>::empty();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut builder = instr::InstructionBuilder::default();
        builder.push_op(instr::OP_CONST_I32);
        builder.push_immediate::<i32>(5);
        builder.push_op(instr::OP_CONST_I32);
        builder.push_immediate::<i32>(1);
        builder.push_op(instr::OP_BIT_AND);
        builder.push_op(instr::OP_RET);
        let code = builder.inner();
        let mut vm = VM {
            world: &world,
            owner: Entity::PLACEHOLDER,
            globals: &globals,
            entity_props: &entity_props,
            entity_methods: &entity_methods,
            stack: Vec::with_capacity(100),
            iptr: code.as_ptr(),
            tracking: RefCell::new(&mut tracking),
        };
        let result = vm.run().unwrap();
        assert_eq!(result, Value::I32(1));
    }

    #[test]
    fn test_bit_or_i32() {
        let world = World::new();
        let globals = SymbolTable::<Global>::empty();
        let entity_props = SymbolTable::<EntityProperty>::empty();
        let entity_methods = SymbolTable::<EntityMethod>::empty();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut builder = instr::InstructionBuilder::default();
        builder.push_op(instr::OP_CONST_I32);
        builder.push_immediate::<i32>(4);
        builder.push_op(instr::OP_CONST_I32);
        builder.push_immediate::<i32>(1);
        builder.push_op(instr::OP_BIT_OR);
        builder.push_op(instr::OP_RET);
        let code = builder.inner();
        let mut vm = VM {
            world: &world,
            owner: Entity::PLACEHOLDER,
            globals: &globals,
            entity_props: &entity_props,
            entity_methods: &entity_methods,
            stack: Vec::with_capacity(100),
            iptr: code.as_ptr(),
            tracking: RefCell::new(&mut tracking),
        };
        let result = vm.run().unwrap();
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
        let mut globals = SymbolTable::<Global>::empty();
        let mut entity_props = SymbolTable::<EntityProperty>::empty();
        let entity_methods = SymbolTable::<EntityMethod>::empty();
        let self_id = globals.insert("self", Global::Property(get_self));
        let health_id = entity_props.insert("health", entity_health);
        let mut tracking = TrackingScope::new(Tick::default());
        let mut builder = instr::InstructionBuilder::default();
        builder.push_op(instr::OP_LOAD_GLOBAL);
        builder.push_immediate::<u32>(self_id as u32);
        builder.push_op(instr::OP_LOAD_ENTITY_PROP);
        builder.push_immediate::<u32>(health_id as u32);
        builder.push_op(instr::OP_RET);
        let code = builder.inner();
        let mut vm = VM {
            world: &world,
            owner: actor_id,
            globals: &globals,
            entity_props: &entity_props,
            entity_methods: &entity_methods,
            stack: Vec::with_capacity(100),
            iptr: code.as_ptr(),
            tracking: RefCell::new(&mut tracking),
        };
        let result = vm.run().unwrap();
        assert_eq!(result, Value::F32(22.0));
    }

    fn get_self(vm: &VM) -> Result<Value, VMError> {
        Ok(Value::Entity(vm.owner))
    }

    fn entity_health(vm: &VM, actor: Entity) -> Result<Value, VMError> {
        let entity = vm.world.entity(actor);
        if let Some(&Health(h)) = entity.get::<Health>() {
            vm.tracking
                .borrow_mut()
                .track_component::<Health>(actor, vm.world, true);
            Ok(Value::F32(h))
        } else {
            vm.tracking
                .borrow_mut()
                .track_component::<Health>(actor, vm.world, false);
            Err(VMError::MissingComponent(Health.type_id()))
        }
    }
}
