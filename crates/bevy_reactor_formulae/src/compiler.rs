use std::sync::Mutex;

use crate::{
    Module, expr::Expr, expr_type::ExprType, host::HostState, location::TokenLocation,
    oper::BinaryOp, parser::formula_parser, pass,
};
use bumpalo::Bump;
use peg::{error::ParseError, str::LineCol};
use smol_str::SmolStr;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum CompilationError {
    #[error("Expression expected")]
    ExpectExpression(TokenLocation),
    #[error("Declaration expected")]
    ExpectDeclaration(TokenLocation),
    #[error("Statement expected")]
    ExpectStatement(TokenLocation),
    #[error("Semicolon expected")]
    ExpectSemi(TokenLocation),
    #[error("Expected {1}")]
    Expected(TokenLocation, SmolStr),
    #[error("Cannot assign type {2} to {1}")]
    MismatchedTypes(TokenLocation, ExprType, ExprType),
    #[error("Cannot convert type from {2} to {1}")]
    InvalidCast(TokenLocation, ExprType, ExprType),
    #[error("Call expression requires function")]
    NotCallable(TokenLocation),
    #[error("Function expects {1} arguments, got {2}")]
    IncorrectNumberOfArguments(TokenLocation, usize, usize),
    #[error("Recursive type: {1}")]
    RecursiveType(TokenLocation, ExprType),
    #[error("Cannot infer type for '{1}'")]
    MissingType(TokenLocation, SmolStr),
    #[error("Unknown type: {1}")]
    UnknownType(TokenLocation, SmolStr),
    #[error("Unknown field: {1}.{2}")]
    UnknownField(TokenLocation, SmolStr, SmolStr),
    #[error("Invalid element index: {1}.{2}")]
    InvalidIndex(TokenLocation, String, usize),
    #[error("Type {1} does not have fields")]
    NoFields(TokenLocation, ExprType),
    #[error("Type {1} does not have indexed elements")]
    NoIndex(TokenLocation, ExprType),
    #[error("Can't find the name '{1}' in this scope")]
    NoMembers(TokenLocation, SmolStr),
    #[error("Name {1} does not have fields or properties")]
    UnknownSymbol(TokenLocation, String),
    #[error("Invalid type for binary operator {2} to {3}")]
    InvalidBinaryOpType(TokenLocation, BinaryOp, ExprType, ExprType),
    #[error("Cannot assign to this expression")]
    InvalidAssignmentTarget(TokenLocation),
    #[error("Function redefinition: {1}")]
    FunctionRedefinition(TokenLocation, String),
    #[error("Name redefinition: {1}")]
    NameRedefinition(TokenLocation, String),
    #[error("Native function may not have a body")]
    NativeFunctionHasBody(TokenLocation),
    #[error("Function without a body")]
    MissingBody(TokenLocation),
}

impl CompilationError {
    pub fn location(&self) -> TokenLocation {
        match self {
            CompilationError::ExpectExpression(loc)
            | CompilationError::ExpectDeclaration(loc)
            | CompilationError::ExpectStatement(loc)
            | CompilationError::ExpectSemi(loc)
            | CompilationError::Expected(loc, _)
            | CompilationError::MismatchedTypes(loc, _, _)
            | CompilationError::InvalidCast(loc, _, _)
            | CompilationError::NotCallable(loc)
            | CompilationError::IncorrectNumberOfArguments(loc, _, _)
            | CompilationError::RecursiveType(loc, _)
            | CompilationError::MissingType(loc, _)
            | CompilationError::UnknownType(loc, _)
            | CompilationError::NoFields(loc, _)
            | CompilationError::NoIndex(loc, _)
            | CompilationError::NoMembers(loc, _)
            | CompilationError::UnknownSymbol(loc, _)
            | CompilationError::UnknownField(loc, _, _)
            | CompilationError::InvalidIndex(loc, _, _)
            | CompilationError::InvalidBinaryOpType(loc, _, _, _)
            | CompilationError::InvalidAssignmentTarget(loc)
            | CompilationError::FunctionRedefinition(loc, _)
            | CompilationError::NameRedefinition(loc, _)
            | CompilationError::NativeFunctionHasBody(loc)
            | CompilationError::MissingBody(loc) => *loc,
        }
    }
}

#[derive(Default, Debug)]
pub struct CompiledFunction {
    pub(crate) code: Vec<u8>,
    pub(crate) num_locals: usize,
}

#[derive(Default, Debug)]
pub(crate) struct FunctionBody<'ex> {
    pub(crate) body: Option<&'ex Expr<'ex>>,
    pub(crate) num_params: usize,
    pub(crate) locals: Vec<ExprType>,
    // locals: Vec<Decl2>,
}

#[derive(Default, Debug)]
pub(crate) struct ModuleExprs<'ex> {
    pub(crate) functions: Vec<FunctionBody<'ex>>,
}

// impl<'cu, 'host> CompilationUnit<'cu, 'host> {
//     pub fn filename(&self) -> &'cu str {
//         Path::new(self.path).file_name().unwrap().to_str().unwrap()
//     }

//     // pub fn add_imports(&mut self, src: &str) -> Result<(), CompilationError> {
//     //     let arena = bumpalo::Bump::new();
//     //     let ast = self.parse_file(src, &arena)?;

//     //     pass::define_imports(&mut self.decls, ast)?;
//     //     // let mut root_scope = Scope::new(Some(&self.intrinsic_scope));
//     //     // self.resolve_imports().await?;

//     //     Ok(())
//     // }

//     // / Load and resolve imported symbols from other modules.
//     // async fn resolve_imports(&mut self) -> Result<(), CompilationError> {
//     //     // TODO: Implement
//     //     Ok(())
//     // }
// }

/// Print out the error with the source token highlighted.
pub fn report_error(path: &str, src: &str, err: &CompilationError) {
    let location = err.location();
    let mut line_ct = 1;
    let mut offset = 0;
    for line in src.lines() {
        let end_offset = offset + line.len();
        let token_end = location.end().min(end_offset);
        if offset <= location.start() && location.start() < end_offset {
            eprintln!(
                "{}:{}:{} {}",
                path,
                line_ct,
                location.start() - offset + 1,
                err
            );
            eprintln!("{line}");
            eprintln!(
                "{}{}",
                " ".repeat(location.start() - offset),
                "^".repeat(token_end - location.start())
            );
            return;
        }
        line_ct += 1;
        offset = end_offset + 1;
    }

    eprintln!("{}:{:?}:{}", path, location.start(), err);
}

fn transform_parse_error(err: ParseError<LineCol>) -> CompilationError {
    let location = TokenLocation::new(err.location.offset, err.location.offset + 1);
    // TODO: Move error handling out
    for token in err.expected.tokens() {
        match token {
            "expression" => return CompilationError::ExpectExpression(location),
            "declaration" => return CompilationError::ExpectDeclaration(location),
            "statement" => return CompilationError::ExpectStatement(location),
            "semicolon" => return CompilationError::ExpectSemi(location),
            _ => {}
        }
    }
    let tokens = err.expected.to_string();
    CompilationError::Expected(location, tokens.into())
}

/// Given a string of source code, compile a module.
pub async fn compile_module(
    path: &str,
    src: &str,
    host: &Mutex<HostState>,
) -> Result<Module, CompilationError> {
    let mut module = Module {
        path: path.to_string(),
        ..Default::default()
    };
    let ast_arena = bumpalo::Bump::new();
    let ast = formula_parser::module(src, &ast_arena).map_err(transform_parse_error)?;

    // pass::define_imports(&mut self.decls, ast)?;
    // self.resolve_imports().await?;
    let expr_arena = Bump::new();
    let host_lock = host.lock().unwrap();
    let module_exprs = pass::build_module_exprs(&host_lock, &mut module, ast, &expr_arena)?;
    pass::gen_module(&mut module, &module_exprs)?;
    Ok(module)
}

/// Given a string of source code, compile a formula. A formula is a module which allows executable
/// statements at the root level.
pub async fn compile_formula(
    path: &str,
    src: &str,
    host: &HostState,
    result_type: ExprType,
) -> Result<Module, CompilationError> {
    let mut module = Module {
        path: path.to_string(),
        ..Default::default()
    };
    let ast_arena = bumpalo::Bump::new();
    let ast = formula_parser::formula(src, &ast_arena).map_err(transform_parse_error)?;

    // pass::define_imports(&mut self.decls, ast)?;
    // self.resolve_imports().await?;
    let expr_arena = Bump::new();
    let module_exprs = pass::build_formula_exprs(host, &mut module, ast, result_type, &expr_arena)?;
    pass::gen_module(&mut module, &module_exprs)?;
    Ok(module)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        VM, Value,
        expr_type::{ExprType, Param},
        host::HostState,
        instr::disassemble,
        vm::VMError,
    };
    use bevy::{
        ecs::{
            component::{Component, Tick},
            entity::Entity,
            world::World,
        },
        math::Vec3,
        reflect::{Reflect, Typed},
    };
    use bevy_reactor::TrackingScope;
    use futures_lite::future;
    use smol_str::SmolStr;
    use std::any::Any;

    #[derive(Component)]
    struct Health(f32);

    #[derive(Component)]
    struct Position(Vec3);

    fn get_self(vm: &VM) -> Result<Value, VMError> {
        Ok(Value::Entity(vm.owner))
    }

    fn entity_health(vm: &VM, actor: Value) -> Result<Value, VMError> {
        let Value::Entity(entity) = actor else {
            panic!("Not an entity");
        };
        if let Some(&Health(h)) = vm.component::<Health>(entity) {
            Ok(Value::F32(h))
        } else {
            Err(VMError::MissingComponent(Health.type_id()))
        }
    }

    fn entity_position(vm: &VM, actor: Value) -> Result<Value, VMError> {
        let Value::Entity(entity) = actor else {
            panic!("Not an entity");
        };
        if let Some(Position(h)) = vm.component::<Position>(entity) {
            Ok(vm.create_world_ref(h.as_reflect()))
        } else {
            Err(VMError::MissingComponent(Health.type_id()))
        }
    }

    #[test]
    fn compile_int_lit() {
        let host = HostState::default();
        let module =
            future::block_on(compile_formula("--str--", "20", &host, ExprType::I32)).unwrap();

        let world = World::new();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut vm = VM::new(&world, &host, &mut tracking);
        let result = vm.run(&module, Module::DEFAULT).unwrap();
        assert_eq!(result, Value::I32(20));
    }

    #[test]
    fn compile_addition() {
        let host = HostState::default();
        let module =
            future::block_on(compile_formula("--str--", "20 + 10", &host, ExprType::I32)).unwrap();

        let world = World::new();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut vm = VM::new(&world, &host, &mut tracking);
        let result = vm.run(&module, Module::DEFAULT).unwrap();
        assert_eq!(result, Value::I32(30));
    }

    #[test]
    fn compile_addition_f32() {
        let host = HostState::default();
        let module = future::block_on(compile_formula(
            "--str--",
            "20.0 + 10.0",
            &host,
            ExprType::F32,
        ))
        .unwrap();

        let world = World::new();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut vm = VM::new(&world, &host, &mut tracking);
        let result = vm.run(&module, Module::DEFAULT).unwrap();
        assert_eq!(result, Value::F32(30.0));
    }

    #[test]
    fn compile_mismatched_types() {
        let host = HostState::default();
        let error = future::block_on(compile_formula(
            "--str--",
            "20.0 + 10",
            &host,
            ExprType::F32,
        ))
        .unwrap_err();
        assert!(matches!(error, CompilationError::MismatchedTypes(_, _, _)));
        assert_eq!(error.location().start(), 7);
        assert_eq!(error.location().end(), 9);
    }

    #[test]
    fn compile_relational_eq() {
        let host = HostState::default();
        let module = future::block_on(compile_formula(
            "--str--",
            "20 == 10",
            &host,
            ExprType::Boolean,
        ))
        .unwrap();

        let world = World::new();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut vm = VM::new(&world, &host, &mut tracking);
        let result = vm.run(&module, Module::DEFAULT).unwrap();
        assert_eq!(result, Value::Bool(false));
    }

    #[test]
    fn compile_relational_ne() {
        let host = HostState::default();
        let module = future::block_on(compile_formula(
            "--str--",
            "20 != 10",
            &host,
            ExprType::Boolean,
        ))
        .unwrap();

        let world = World::new();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut vm = VM::new(&world, &host, &mut tracking);
        let result = vm.run(&module, Module::DEFAULT).unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn compile_relational_lt() {
        let host = HostState::default();
        let module = future::block_on(compile_formula(
            "--str--",
            "20 < 10",
            &host,
            ExprType::Boolean,
        ))
        .unwrap();

        let world = World::new();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut vm = VM::new(&world, &host, &mut tracking);
        let result = vm.run(&module, Module::DEFAULT).unwrap();
        assert_eq!(result, Value::Bool(false));
    }

    #[test]
    fn compile_relational_le() {
        let host = HostState::default();
        let module = future::block_on(compile_formula(
            "--str--",
            "20 <= 10",
            &host,
            ExprType::Boolean,
        ))
        .unwrap();

        let world = World::new();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut vm = VM::new(&world, &host, &mut tracking);
        let result = vm.run(&module, Module::DEFAULT).unwrap();
        assert_eq!(result, Value::Bool(false));
    }

    #[test]
    fn compile_relational_gt() {
        let host = HostState::default();
        let module = future::block_on(compile_formula(
            "--str--",
            "20 > 10",
            &host,
            ExprType::Boolean,
        ))
        .unwrap();

        let world = World::new();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut vm = VM::new(&world, &host, &mut tracking);
        let result = vm.run(&module, Module::DEFAULT).unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn compile_relational_ge() {
        let host = HostState::default();
        let module = future::block_on(compile_formula(
            "--str--",
            "20 >= 10",
            &host,
            ExprType::Boolean,
        ))
        .unwrap();

        let world = World::new();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut vm = VM::new(&world, &host, &mut tracking);
        let result = vm.run(&module, Module::DEFAULT).unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn compile_entity_method() {
        let mut world = World::new();
        let actor = world.spawn(Health(22.0));
        let actor_id = actor.id();
        let mut host = HostState::default();
        host.add_global_prop("self", get_self, ExprType::Entity);
        host.add_host_type::<Entity>("Entity")
            .add_property("health", entity_health, ExprType::F32);

        let module = future::block_on(compile_formula(
            "--str--",
            "self.health",
            &host,
            ExprType::F32,
        ))
        .unwrap();

        let mut tracking = TrackingScope::new(Tick::default());
        let mut vm = VM::new(&world, &host, &mut tracking);
        vm.owner = actor_id;
        let result = vm.run(&module, Module::DEFAULT).unwrap();
        assert_eq!(result, Value::F32(22.0));
    }

    #[test]
    fn compile_reflected_type() {
        let mut world = World::new();
        let actor = world.spawn(Position(Vec3::new(1.0, 2.0, 3.0)));
        let actor_id = actor.id();
        let mut host = HostState::default();
        host.add_global_prop("self", get_self, ExprType::Entity);
        host.add_host_type::<Entity>("Entity").add_property(
            "position",
            entity_position,
            ExprType::Reflected(Vec3::type_info()),
        );

        let module = future::block_on(compile_formula(
            "--str--",
            "self.position.x",
            &host,
            ExprType::F32,
        ))
        .unwrap();
        // disassemble(&module.functions[0].code);

        let mut tracking = TrackingScope::new(Tick::default());
        let mut vm = VM::new(&world, &host, &mut tracking);
        vm.owner = actor_id;
        let result = vm.run(&module, Module::DEFAULT).unwrap();
        assert_eq!(result, Value::F32(1.0));
    }

    #[test]
    fn compile_block_result() {
        let host = HostState::default();
        let module =
            future::block_on(compile_formula("--str--", "{ 20 }", &host, ExprType::I32)).unwrap();

        let world = World::new();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut vm = VM::new(&world, &host, &mut tracking);
        let result = vm.run(&module, Module::DEFAULT).unwrap();
        assert_eq!(result, Value::I32(20));
    }

    #[test]
    fn compile_if_else() {
        let mut world = World::new();
        let actor = world.spawn(Health(22.0));
        let actor_id = actor.id();
        let mut host = HostState::default();
        host.add_global_prop("self", get_self, ExprType::Entity);
        host.add_host_type::<Entity>("Entity")
            .add_property("health", entity_health, ExprType::F32);

        let module = future::block_on(compile_formula(
            "--str--",
            "if self.health > 0.0 { 2.0 } else { 3.0 }",
            &host,
            ExprType::F32,
        ))
        .unwrap();

        let mut tracking = TrackingScope::new(Tick::default());
        let mut vm = VM::new(&world, &host, &mut tracking);
        vm.owner = actor_id;
        // eprintln!("Code: {:?}", module.functions[0].code);
        let result = vm.run(&module, Module::DEFAULT).unwrap();
        assert_eq!(result, Value::F32(2.0));
    }

    #[test]
    fn compile_string_method() {
        let host = HostState::new();
        let module = future::block_on(compile_formula(
            "--str--",
            "\"test\".len()",
            &host,
            ExprType::I32,
        ))
        .unwrap();
        // disassemble(&module.functions[0].code);

        let world = World::new();
        let mut tracking = TrackingScope::new(Tick::default());
        let mut vm = VM::new(&world, &host, &mut tracking);
        let result = vm.run(&module, Module::DEFAULT).unwrap();
        assert_eq!(result, Value::I32(4));
    }

    #[test]
    fn compile_module_with_func() {
        let host = Mutex::new(HostState::new());
        let module =
            future::block_on(compile_module("--str--", "fn test() -> i32 { 20 }", &host)).unwrap();

        let world = World::new();
        let mut tracking = TrackingScope::new(Tick::default());
        let host = host.lock().unwrap();
        let mut vm = VM::new(&world, &host, &mut tracking);
        let result = vm.run(&module, "test").unwrap();
        assert_eq!(result, Value::I32(20));
    }

    #[test]
    fn compile_call() {
        let host = Mutex::new(HostState::new());
        let module = future::block_on(compile_module(
            "--str--",
            "
            fn test2() -> i32 { 20 }
            fn test() -> i32 { test2() }
            ",
            &host,
        ))
        .unwrap();

        let world = World::new();
        let mut tracking = TrackingScope::new(Tick::default());
        let host = host.lock().unwrap();
        let mut vm = VM::new(&world, &host, &mut tracking);
        let result = vm.run(&module, "test").unwrap();
        assert_eq!(result, Value::I32(20));
    }

    #[test]
    fn compile_params() {
        let host = Mutex::new(HostState::new());
        let module = future::block_on(compile_module(
            "--str--",
            "
            fn test2(i: i32) -> i32 { 20 + i }
            fn test() -> i32 { test2(5) }
            ",
            &host,
        ))
        .unwrap();

        let world = World::new();
        let mut tracking = TrackingScope::new(Tick::default());
        let host = host.lock().unwrap();
        let mut vm = VM::new(&world, &host, &mut tracking);
        let result = vm.run(&module, "test").unwrap();
        assert_eq!(result, Value::I32(25));
    }

    #[test]
    fn compile_void_fn() {
        let host = Mutex::new(HostState::new());
        let module = future::block_on(compile_module(
            "--str--",
            "
            fn test2(i: i32) {}
            fn test() -> i32 { test2(0); 5 }
            ",
            &host,
        ))
        .unwrap();
        // disassemble(&module.functions[0].code);
        // disassemble(&module.functions[1].code);

        let world = World::new();
        let mut tracking = TrackingScope::new(Tick::default());
        let host = host.lock().unwrap();
        let mut vm = VM::new(&world, &host, &mut tracking);
        let result = vm.run(&module, "test").unwrap();
        assert_eq!(result, Value::I32(5));
    }

    #[test]
    fn compile_local_var() {
        let host = Mutex::new(HostState::new());
        let module = future::block_on(compile_module(
            "--str--",
            "
            fn test() -> i32 {
                let x = 19;
                let y = 1;
                x + y
            }
            ",
            &host,
        ))
        .unwrap();
        // disassemble(&module.functions[0].code);

        let world = World::new();
        let mut tracking = TrackingScope::new(Tick::default());
        let host = host.lock().unwrap();
        let mut vm = VM::new(&world, &host, &mut tracking);
        let result = vm.run(&module, "test").unwrap();
        assert_eq!(result, Value::I32(20));
    }

    #[test]
    fn compile_constructor() {
        let mut host = HostState::new();
        host.add_host_type::<Vec3>("Vec3");
        host.add_static_method::<Vec3>(
            "new",
            vec3_new,
            vec![
                Param {
                    name: SmolStr::new_static("x"),
                    ty: ExprType::F32,
                },
                Param {
                    name: SmolStr::new_static("y"),
                    ty: ExprType::F32,
                },
                Param {
                    name: SmolStr::new_static("z"),
                    ty: ExprType::F32,
                },
            ],
            ExprType::Reflected(Vec3::type_info()),
        );
        let host = Mutex::new(host);
        let module = future::block_on(compile_module(
            "--str--",
            "
            fn test() -> f32 { Vec3::new(1.0, 2.0, 3.0).x }
            ",
            &host,
        ))
        .unwrap();
        // disassemble(&module.functions[0].code);
        // disassemble(&module.functions[1].code);

        let world = World::new();
        let mut tracking = TrackingScope::new(Tick::default());
        let host = host.lock().unwrap();
        let mut vm = VM::new(&world, &host, &mut tracking);
        let result = vm.run(&module, "test").unwrap();
        assert_eq!(result, Value::F32(1.0));
    }

    fn vec3_new(vm: &VM, args: &[Value]) -> Result<Value, VMError> {
        assert_eq!(args.len(), 3);
        let Value::F32(x) = args[0] else {
            return Err(VMError::MismatchedTypes(
                vm.value_type(&args[0]),
                ExprType::F32,
            ));
        };
        let Value::F32(y) = args[1] else {
            return Err(VMError::MismatchedTypes(
                vm.value_type(&args[1]),
                ExprType::F32,
            ));
        };
        let Value::F32(z) = args[2] else {
            return Err(VMError::MismatchedTypes(
                vm.value_type(&args[2]),
                ExprType::F32,
            ));
        };
        let result = Vec3 { x, y, z };
        Ok(vm.create_heap_ref(result))
    }
}
