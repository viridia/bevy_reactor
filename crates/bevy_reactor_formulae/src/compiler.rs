use std::sync::Mutex;

use crate::{
    Module, expr::Expr, expr_type::ExprType, host::HostState, location::TokenLocation,
    oper::BinaryOp, parser::formula_parser, pass,
};
use bumpalo::Bump;
use peg::{error::ParseError, str::LineCol};
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
    Expected(TokenLocation, String),
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
    MissingType(TokenLocation, String),
    #[error("Unknown type: {1}")]
    UnknownType(TokenLocation, String),
    #[error("Unknown field: {1}.{2}")]
    UnknownField(TokenLocation, String, String),
    #[error("Invalid element index: {1}.{2}")]
    InvalidIndex(TokenLocation, String, usize),
    #[error("Type {1} does not have fields")]
    NoFields(TokenLocation, ExprType),
    #[error("Type {1} does not have indexed elements")]
    NoIndex(TokenLocation, ExprType),
    #[error("Can't find the name '{1}' in this scope")]
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
}

#[derive(Default, Debug)]
pub(crate) struct FunctionBody<'ex> {
    pub(crate) body: Option<&'ex Expr<'ex>>,
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

//     // pub(crate) fn define_intrinsics(&mut self) {
//     //     let symbols = &self.decls.symbols;
//     //     let intrinsic_scope = &mut self.intrinsic_scope;
//     //     intrinsic_scope.insert(symbols.intern("i32"), Decl::TypeAlias(ExprType::I32));
//     //     intrinsic_scope.insert(symbols.intern("i64"), Decl::TypeAlias(ExprType::I64));
//     //     intrinsic_scope.insert(symbols.intern("f32"), Decl::TypeAlias(ExprType::F32));
//     //     intrinsic_scope.insert(symbols.intern("f64"), Decl::TypeAlias(ExprType::F64));
//     //     intrinsic_scope.insert(symbols.intern("bool"), Decl::TypeAlias(ExprType::Boolean));
//     //     intrinsic_scope.insert(symbols.intern("String"), Decl::TypeAlias(ExprType::String));
//     // }

//     fn parse_file<'ast>(
//         &mut self,
//         src: &str,
//         arena: &'ast Bump,
//     ) -> Result<&'ast ASTNode<'ast>, CompilationError> {
//         let ast = formula_parser::module(src, arena).map_err(transform_parse_error)?;

//         Ok(ast)
//     }

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
    CompilationError::Expected(location, tokens)
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
    use crate::{VM, Value, expr_type::ExprType, host::HostState, vm::VMError};
    use bevy::ecs::{
        component::{Component, Tick},
        entity::Entity,
        world::World,
    };
    use bevy_reactor::TrackingScope;
    use futures_lite::future;
    use std::any::Any;

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
        host.add_entity_prop("health", entity_health, ExprType::F32);

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
        host.add_entity_prop("health", entity_health, ExprType::F32);

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

    #[derive(Component)]
    struct Health(f32);

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
}
