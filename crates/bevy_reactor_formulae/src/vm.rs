use std::{
    any::{Any, TypeId},
    cell::RefCell,
    ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Sub},
    sync::Arc,
};

use bevy::{
    ecs::{component::Component, entity::Entity, world::World},
    reflect::{PartialReflect, ReflectRef},
};
use bevy_reactor::TrackingScope;
use thiserror::Error;

use crate::{
    Module,
    decl::DeclKind,
    expr_type::ExprType,
    host::{Global, HostState, HostTypeMember},
    instr,
};

// Values on the stack
#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Void,
    Bool(bool),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    Entity(Entity),
    String(Arc<String>),
    WorldRef(usize),
    HeapRef(u16),
}

impl Value {
    pub fn is_void(&self) -> bool {
        *self == Value::Void
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
    #[error("Invalid type for operation: {0}")]
    InvalidType(ExprType),
    #[error("Invalid global index: {0}")]
    InvalidGlobalIndex(usize),
    #[error("Invalid entity prop: {0}")]
    InvalidNativeProp(usize),
    #[error("Invalid entity method: {0}")]
    InvalidNativeMethod(usize),
    #[error("Invalid method: {0}")]
    InvalidMethod(usize),
    #[error("Function not found: {0}")]
    MissingFunction(String),
    #[error("Missing Component: {0:?}")]
    MissingComponent(TypeId),
    #[error("Function {0} expects {1} parameters, got {2}")]
    IncorrectParamCount(String, usize, usize),
}

/// Saved state of VM for nested function calls
struct CallStackEntry {
    /// Module we are executing
    pub module: *const Module,

    /// Value stack
    stack: Vec<Value>,

    /// Instruction pointer
    iptr: *const u8,
}

/// A virtual machine for evaluating formulae.
pub struct VM<'world, 'host, 'p> {
    /// Bevy World
    pub world: &'world World,

    /// Entity upon which this script is attached.
    pub owner: Entity,

    /// Globals and entity members
    pub host: &'host HostState,

    /// Module we are executing
    pub module: *const Module,

    /// Execution stack
    call_stack: Vec<CallStackEntry>,

    /// Reflected values.
    /// TODO: Need garbage collection
    /// TODO: Would be better to use an arena for this and other temporaries.
    world_refs: RefCell<Vec<&'world dyn PartialReflect>>,

    /// Temporary non-primitive values created during execution.
    heap_refs: RefCell<Vec<Box<dyn PartialReflect>>>,

    /// Value stack: consists of [Value; num_params + num_locals + temporaries]
    stack: Vec<Value>,

    /// Instruction pointer
    iptr: *const u8,

    /// Reactive tracking scope
    pub tracking: RefCell<&'p mut TrackingScope>,
}

type InstrHandler = fn(&mut VM) -> Result<(), VMError>;

impl<'world, 'host, 'p> VM<'world, 'host, 'p> {
    const JUMP_TABLE: [InstrHandler; 256] = build_jump_table();

    // Initialize a new virtual machine.
    pub fn new(
        world: &'world World,
        host: &'host HostState,
        tracking: &'p mut TrackingScope,
    ) -> Self {
        Self {
            world,
            owner: Entity::PLACEHOLDER,
            host,
            module: Default::default(),
            call_stack: Vec::new(),
            world_refs: RefCell::new(Vec::new()),
            heap_refs: RefCell::new(Vec::new()),
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
            // eprintln!("Opcode: {op}");
            if op == instr::OP_RET {
                let ret_val = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                if self.call_stack.is_empty() {
                    return Ok(ret_val);
                } else {
                    self.pop_call_stack();
                    if !ret_val.is_void() {
                        self.stack.push(ret_val);
                    }
                    continue;
                }
            }
            self.iptr = unsafe { self.iptr.add(1) };
            VM::JUMP_TABLE[op as usize](self)?;
        }
    }

    /// Run a compiled function by name.
    pub fn run(&mut self, module: &crate::Module, function_name: &str) -> Result<Value, VMError> {
        if let Some(entry_fn) = module.module_decls.get(function_name) {
            if let DeclKind::Function { index, .. } = &entry_fn.kind {
                let ExprType::Function(ftype) = &entry_fn.typ else {
                    panic!("Function should have a function type: {:?}.", entry_fn.typ)
                };
                if !ftype.params.is_empty() {
                    return Err(VMError::IncorrectParamCount(
                        function_name.to_string(),
                        ftype.params.len(),
                        0,
                    ));
                }
                let function = &module.functions[*index];
                self.module = module;
                self.stack.resize(function.num_locals, Value::Void);
                self.iptr = function.code.as_ptr();
            }
            self.start()
        } else {
            Err(VMError::MissingFunction(function_name.to_string()))
        }
    }

    /// Run a compiled function by name with arguments.
    pub fn run_with(
        &mut self,
        module: &crate::Module,
        function_name: &str,
        params: &[Value],
    ) -> Result<Value, VMError> {
        if let Some(entry_fn) = module.module_decls.get(function_name) {
            if let DeclKind::Function { index, .. } = &entry_fn.kind {
                let ExprType::Function(ftype) = &entry_fn.typ else {
                    panic!("Function should have a function type: {:?}.", entry_fn.typ)
                };
                if ftype.params.len() != params.len() {
                    return Err(VMError::IncorrectParamCount(
                        function_name.to_string(),
                        ftype.params.len(),
                        params.len(),
                    ));
                }
                let function = &module.functions[*index];
                self.module = module;
                self.stack.extend_from_slice(params);
                self.stack
                    .resize(self.stack.len() + function.num_locals, Value::Void);
                self.iptr = function.code.as_ptr();
            }
            self.start()
        } else {
            Err(VMError::MissingFunction(function_name.to_string()))
        }
    }

    /// Return a reference to the Component `C` on the given entity. Calling this function
    /// adds the component as a dependency of the current tracking scope.
    pub fn component<C: Component>(&self, entity: Entity) -> Option<&'world C> {
        let comp = self.world.entity(entity).get::<C>();
        self.tracking
            .borrow_mut()
            .track_component::<C>(entity, self.world, comp.is_some());
        comp
    }

    /// Construct a `Value` containing a `PartialReflect` taken from the Bevy world.
    pub fn create_world_ref(&self, reflected: &'world dyn PartialReflect) -> Value {
        let mut world_refs = self.world_refs.borrow_mut();
        let index = world_refs.len();
        world_refs.push(reflected);
        Value::WorldRef(index)
    }

    /// Construct a `Value` containing a `PartialReflect` taken from the Bevy world.
    pub fn create_heap_ref<T: PartialReflect>(&self, value: T) -> Value {
        let mut heap = self.heap_refs.borrow_mut();
        let index = heap.len();
        if index > u16::MAX as usize {
            panic!("VM only supports 64k heap items");
        }
        heap.push(Box::new(value).into_partial_reflect());
        Value::HeapRef(index as u16)
    }

    fn push_call_stack(&mut self, new_stack: Vec<Value>) {
        self.call_stack.push(CallStackEntry {
            module: self.module,
            stack: std::mem::replace(&mut self.stack, new_stack),
            iptr: self.iptr,
        });
    }

    fn pop_call_stack(&mut self) {
        let tos = self.call_stack.pop().unwrap();
        self.module = tos.module;
        self.stack = tos.stack;
        self.iptr = tos.iptr;
    }

    /// Determine the type of this value, if possible.
    pub(crate) fn value_type(&self, value: &Value) -> ExprType {
        match value {
            Value::Void => ExprType::Void,
            Value::Bool(_) => ExprType::Boolean,
            Value::I32(_) => ExprType::I32,
            Value::I64(_) => ExprType::I64,
            Value::F32(_) => ExprType::F32,
            Value::F64(_) => ExprType::F64,
            Value::Entity(_) => ExprType::Entity,
            Value::String(_) => ExprType::String,
            Value::WorldRef(world_index) => {
                if let Some(reflect) =
                    self.world_refs.borrow()[*world_index].get_represented_type_info()
                {
                    ExprType::Reflected(reflect)
                } else {
                    ExprType::None
                }
            }
            Value::HeapRef(heap_index) => {
                if let Some(reflect) =
                    self.heap_refs.borrow()[*heap_index as usize].get_represented_type_info()
                {
                    ExprType::Reflected(reflect)
                } else {
                    ExprType::None
                }
            }
        }
    }
}

// Build the jump table
const fn build_jump_table() -> [InstrHandler; 256] {
    let mut table: [InstrHandler; 256] = [invalid; 256];

    table[instr::OP_CONST_VOID as usize] = const_void;
    table[instr::OP_CONST_TRUE as usize] = const_true;
    table[instr::OP_CONST_FALSE as usize] = const_false;
    table[instr::OP_CONST_I32 as usize] = const_i32;
    table[instr::OP_CONST_I64 as usize] = const_i64;
    table[instr::OP_CONST_F32 as usize] = const_f32;
    table[instr::OP_CONST_F64 as usize] = const_f64;
    table[instr::OP_CONST_STR as usize] = const_str;
    table[instr::OP_DROP as usize] = drop1;
    table[instr::OP_DROP_N as usize] = drop_n;
    table[instr::OP_LOAD_PARAM as usize] = load_param;
    table[instr::OP_LOAD_LOCAL as usize] = load_local;
    table[instr::OP_LOAD_GLOBAL as usize] = load_global;
    table[instr::OP_LOAD_NATIVE_PROP as usize] = load_native_prop;
    table[instr::OP_LOAD_FIELD as usize] = load_field;
    table[instr::OP_STORE_LOCAL as usize] = store_local;

    table[instr::OP_LOGICAL_AND as usize] = log_and;
    table[instr::OP_LOGICAL_OR as usize] = log_or;

    table[instr::OP_RET as usize] = ret;
    table[instr::OP_BRANCH as usize] = branch;
    table[instr::OP_BRANCH_IF_FALSE as usize] = branch_if_false;

    table[instr::OP_CALL as usize] = call;
    table[instr::OP_CALL_HOST_METHOD as usize] = call_host_method;
    table[instr::OP_CALL_HOST_FUNCTION as usize] = call_host_function;

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

fn invalid(vm: &mut VM) -> Result<(), VMError> {
    eprintln!("Invalid instruction: {}", unsafe { *vm.iptr.offset(-1) });
    Err(VMError::InvalidInstruction)
}

fn const_void(vm: &mut VM) -> Result<(), VMError> {
    vm.stack.push(Value::Void);
    Ok(())
}

fn const_true(vm: &mut VM) -> Result<(), VMError> {
    vm.stack.push(Value::Bool(true));
    Ok(())
}

fn const_false(vm: &mut VM) -> Result<(), VMError> {
    vm.stack.push(Value::Bool(false));
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

fn const_str(vm: &mut VM) -> Result<(), VMError> {
    let len = vm.read_immediate::<u16>() as usize;
    let s = unsafe { std::slice::from_raw_parts(vm.iptr, len) };
    vm.iptr = unsafe { vm.iptr.add(len) };
    vm.stack.push(Value::String(Arc::new(unsafe {
        std::str::from_utf8_unchecked(s).to_string()
    })));
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

fn load_param(vm: &mut VM) -> Result<(), VMError> {
    let n = vm.read_immediate::<u32>() as usize;
    let val = vm.stack[n].clone();
    vm.stack.push(val);
    Ok(())
}

fn load_local(vm: &mut VM) -> Result<(), VMError> {
    let n = vm.read_immediate::<u16>() as usize;
    let val = vm.stack[n].clone();
    vm.stack.push(val);
    Ok(())
}

fn store_local(vm: &mut VM) -> Result<(), VMError> {
    let n = vm.read_immediate::<u16>() as usize;
    let arg = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
    vm.stack[n] = arg;
    Ok(())
}

fn load_global(vm: &mut VM) -> Result<(), VMError> {
    let n = vm.read_immediate::<u16>() as usize;
    let val = match vm.host.globals.get(n) {
        Some(Global::Const(val)) => val.clone(),
        Some(Global::Property(accessor)) => accessor(vm)?,
        Some(Global::Function(_)) => return Err(VMError::InvalidNativeProp(n)),
        None => return Err(VMError::InvalidGlobalIndex(n)),
    };
    vm.stack.push(val);
    Ok(())
}

fn load_native_prop(vm: &mut VM) -> Result<(), VMError> {
    let arg = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
    let prop_index = vm.read_immediate::<u16>() as usize;

    let host_type = match arg {
        Value::String(_) => vm
            .host
            .get_host_type::<String>()
            .expect("String type not registered"),
        Value::Entity(_) => vm
            .host
            .get_host_type::<Entity>()
            .expect("Entity type not registered"),
        Value::WorldRef(wref) => vm
            .host
            .get_host_type_by_id(vm.world_refs.borrow()[wref].type_id())
            .expect("Unknown host type"),
        _ => {
            panic!("Type does not have properties");
        }
    };

    let val = match host_type.members.get(prop_index) {
        Some(HostTypeMember::Property(accessor)) => accessor(vm, arg)?,
        Some(HostTypeMember::Method(_)) | Some(HostTypeMember::StaticMethod(_)) => {
            return Err(VMError::InvalidNativeProp(prop_index));
        }
        None => return Err(VMError::InvalidNativeProp(prop_index)),
    };
    vm.stack.push(val);
    Ok(())
}

fn load_field(vm: &mut VM) -> Result<(), VMError> {
    let arg = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
    if let Value::WorldRef(world_index) = arg {
        let field_index = vm.read_immediate::<u16>() as usize;
        let reflect = vm.world_refs.borrow()[world_index];
        let reflect_ref = reflect.reflect_ref();
        match reflect_ref {
            ReflectRef::Struct(rstruct) => {
                let field = rstruct.field_at(field_index);
                let Some(field) = field else {
                    panic!("Invalid struct field index");
                };
                let val = if let Some(val) = field.try_downcast_ref::<i32>() {
                    Value::I32(*val)
                } else if let Some(val) = field.try_downcast_ref::<i64>() {
                    Value::I64(*val)
                } else if let Some(val) = field.try_downcast_ref::<f32>() {
                    Value::F32(*val)
                } else if let Some(val) = field.try_downcast_ref::<f64>() {
                    Value::F64(*val)
                } else {
                    vm.create_world_ref(field)
                };
                vm.stack.push(val);
                Ok(())
            }
            _ => Err(VMError::InvalidType(vm.value_type(&arg))),
        }
    } else if let Value::HeapRef(heap_index) = arg {
        let field_index = vm.read_immediate::<u16>() as usize;
        let heap = vm.heap_refs.borrow();
        let reflect = heap[heap_index as usize].as_ref();
        let reflect_ref = reflect.reflect_ref();
        match reflect_ref {
            ReflectRef::Struct(rstruct) => {
                let field = rstruct.field_at(field_index);
                let Some(field) = field else {
                    panic!("Invalid struct field index");
                };
                let val = if let Some(val) = field.try_downcast_ref::<i32>() {
                    Value::I32(*val)
                } else if let Some(val) = field.try_downcast_ref::<i64>() {
                    Value::I64(*val)
                } else if let Some(val) = field.try_downcast_ref::<f32>() {
                    Value::F32(*val)
                } else if let Some(val) = field.try_downcast_ref::<f64>() {
                    Value::F64(*val)
                } else {
                    todo!();
                    // vm.create_world_ref(field)
                };
                vm.stack.push(val);
                Ok(())
            }
            _ => Err(VMError::InvalidType(vm.value_type(&arg))),
        }
    } else {
        Err(VMError::InvalidType(vm.value_type(&arg)))
    }
}

macro_rules! impl_typed_binop {
    ($name:ident, $variant:ident, $type:ty, $op:ident) => {
        fn $name(vm: &mut VM) -> Result<(), VMError> {
            let rhs = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
            let lhs = vm.stack.last().ok_or(VMError::StackUnderflow)?;
            if let (Value::$variant(l), Value::$variant(r)) = (lhs.clone(), rhs.clone()) {
                *vm.stack.last_mut().ok_or(VMError::StackUnderflow)? = Value::$variant(l.$op(r));
                Ok(())
            } else {
                Err(VMError::MismatchedTypes(
                    vm.value_type(&lhs),
                    vm.value_type(&rhs),
                ))
            }
        }
    };
}

macro_rules! impl_relational_binop {
    ($name:ident, $variant:ident, $type:ty, $op:ident) => {
        fn $name(vm: &mut VM) -> Result<(), VMError> {
            let rhs = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
            let lhs = vm.stack.last().ok_or(VMError::StackUnderflow)?;
            if let (Value::$variant(l), Value::$variant(r)) = (lhs.clone(), rhs.clone()) {
                *vm.stack.last_mut().ok_or(VMError::StackUnderflow)? = Value::Bool(l.$op(&r));
                Ok(())
            } else {
                Err(VMError::MismatchedTypes(
                    vm.value_type(&lhs),
                    vm.value_type(&rhs),
                ))
            }
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
            return Err(VMError::MismatchedTypes(
                vm.value_type(&v0),
                vm.value_type(&v1),
            ));
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
            return Err(VMError::MismatchedTypes(
                vm.value_type(&v0),
                vm.value_type(&v1),
            ));
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

fn call(vm: &mut VM) -> Result<(), VMError> {
    let fn_index = vm.read_immediate::<u16>() as usize;
    let num_params = vm.read_immediate::<u16>() as usize;
    let stack_len = vm.stack.len();
    if num_params > stack_len {
        return Err(VMError::StackUnderflow);
    }
    let func = &unsafe { &*vm.module }.functions[fn_index];
    let iptr = func.code.as_ptr();
    let num_locals = func.num_locals;
    let mut new_stack = vm.stack.split_off(vm.stack.len() - num_params);
    new_stack.resize(new_stack.len() + num_locals, Value::Void);
    vm.push_call_stack(new_stack);
    vm.iptr = iptr;
    Ok(())
}

fn call_host_method(vm: &mut VM) -> Result<(), VMError> {
    let method_index = vm.read_immediate::<u16>() as usize;
    let num_params = vm.read_immediate::<u16>() as usize;
    let stack_len = vm.stack.len();
    if num_params > stack_len {
        return Err(VMError::StackUnderflow);
    }

    let this = vm.stack[stack_len - num_params].clone();
    let host_type = match this {
        Value::String(_) => vm
            .host
            .get_host_type::<String>()
            .expect("String type not registered"),
        Value::Entity(_) => vm
            .host
            .get_host_type::<Entity>()
            .expect("Entity type not registered"),
        Value::WorldRef(wref) => vm
            .host
            .get_host_type_by_id(wref.type_id())
            .expect("Unknown host type"),
        _ => {
            panic!("Type does not have methods");
        }
    };

    let args = &vm.stack[stack_len - num_params..stack_len];
    let val = match host_type.members.get(method_index) {
        Some(HostTypeMember::Method(method)) => method(vm, args)?,
        Some(HostTypeMember::StaticMethod(_)) | Some(HostTypeMember::Property(_)) => {
            return Err(VMError::InvalidNativeMethod(method_index))?;
        }
        None => return Err(VMError::InvalidNativeMethod(method_index)),
    };

    // Drop args
    vm.stack.truncate(stack_len - num_params);
    if !val.is_void() {
        vm.stack.push(val);
    }
    Ok(())
}

fn call_host_function(vm: &mut VM) -> Result<(), VMError> {
    let function_index = vm.read_immediate::<u16>() as usize;
    let num_params = vm.read_immediate::<u16>() as usize;
    let stack_len = vm.stack.len();
    if num_params > stack_len {
        return Err(VMError::StackUnderflow);
    }

    let args = &vm.stack[stack_len - num_params..stack_len];
    let val = match vm.host.globals.get(function_index) {
        Some(Global::Function(f)) => f(vm, args)?,
        Some(Global::Const(_)) | Some(Global::Property(_)) => {
            return Err(VMError::InvalidGlobalIndex(function_index))?;
        }
        None => return Err(VMError::InvalidGlobalIndex(function_index)),
    };

    // Drop args
    vm.stack.truncate(stack_len - num_params);
    if !val.is_void() {
        vm.stack.push(val);
    }
    Ok(())
}

fn branch(vm: &mut VM) -> Result<(), VMError> {
    let offset = vm.read_immediate::<i32>();
    vm.iptr = unsafe { vm.iptr.offset(offset as isize) };
    Ok(())
}

fn branch_if_false(vm: &mut VM) -> Result<(), VMError> {
    let test = vm.stack.pop().ok_or(VMError::StackUnderflow)?;
    let offset = vm.read_immediate::<i32>();
    match test {
        Value::Bool(false) => {
            vm.iptr = unsafe { vm.iptr.offset(offset as isize) };
            Ok(())
        }

        Value::Bool(true) => Ok(()),

        _ => Err(VMError::MismatchedTypes(
            vm.value_type(&test),
            ExprType::Boolean,
        )),
    }
}

fn ret(_vm: &mut VM) -> Result<(), VMError> {
    // `ret` is handled by the interpreter loop.
    unreachable!("`ret` instruction handler should not be called");
}

#[cfg(test)]
mod tests {
    use bevy::ecs::component::{Component, Tick};

    use crate::{expr_type::ExprType, instr};

    use super::*;

    #[test]
    fn test_invalid_instr() {
        let world = World::new();
        let host = HostState::default();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut builder = instr::InstructionBuilder::default();
        builder.push_op(0);
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

    #[test]
    fn test_global() {
        let mut world = World::new();
        let actor = world.spawn(());
        let actor_id = actor.id();
        let mut host = HostState::default();
        let self_id = host.add_global_prop("self", get_self, ExprType::Entity);
        host.add_host_type::<Entity>("Entity");
        let mut tracking = TrackingScope::new(Tick::default());
        let mut builder = instr::InstructionBuilder::default();
        builder.push_op(instr::OP_LOAD_GLOBAL);
        builder.push_immediate::<u16>(self_id as u16);
        builder.push_op(instr::OP_RET);
        let code = builder.inner();
        let mut vm = VM::new(&world, &host, &mut tracking);
        vm.owner = actor_id;
        vm.iptr = code.as_ptr();
        let result = vm.start().unwrap();
        assert_eq!(result, Value::Entity(actor_id));
    }

    fn get_self(vm: &VM) -> Result<Value, VMError> {
        Ok(Value::Entity(vm.owner))
    }

    #[test]
    fn test_value_size() {
        assert_eq!(std::mem::size_of::<Value>(), 16);
    }
}
