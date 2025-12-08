use core::fmt::Display;
use std::sync::Arc;

use bevy::{ecs::entity::Entity, reflect::TypeInfo};
use smol_str::SmolStr;

use crate::decl::FunctionParam;

/// Represents the type of an expression.
#[derive(Debug, Default, Clone)]
pub enum ExprType {
    #[default]
    None, // No type specified
    Infer(TypeVarId), // Type to be inferred
    Void,             // No value
    Boolean,
    IUnsized, // Integer constant with no explicit type
    I32,
    I64,
    F32,
    F64,
    String,
    Entity,
    // Tuple(Arc<[ExprType]>),
    // Array(Arc<ExprType>),
    Function(Arc<FunctionType>),
    Reflected(&'static TypeInfo),
    // Struct(Arc<StructType>),
    // TupleStruct(Arc<TupleStructType>),
    // TODO: Struct, Record, Option, Enum
}

impl ExprType {
    pub fn from_type_info(type_info: &'static TypeInfo) -> Self {
        if type_info.is::<bool>() {
            ExprType::Boolean
        } else if type_info.is::<i32>() {
            ExprType::I32
        } else if type_info.is::<i64>() {
            ExprType::I64
        } else if type_info.is::<f32>() {
            ExprType::F32
        } else if type_info.is::<f64>() {
            ExprType::F64
        } else if type_info.is::<Entity>() {
            ExprType::Entity
        } else {
            ExprType::Reflected(type_info)
        }
    }
}

impl PartialEq for ExprType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Infer(l0), Self::Infer(r0)) => l0 == r0,
            (Self::Function(l0), Self::Function(r0)) => l0 == r0,
            (Self::Reflected(_l0), Self::Reflected(_r0)) => todo!(),
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

/// Type data for a function
#[derive(Debug, Default, PartialEq, Clone)]
pub struct FunctionType {
    pub params: Vec<FunctionParam>,
    pub ret: ExprType,
}

// / Type data for a struct or record
// #[derive(Debug, PartialEq, Default, Clone)]
// pub struct StructType {
//     pub name: Symbol,
//     pub fields: Vec<FieldDecl>,
// }

// / Type data for a tuple struct or record
// #[derive(Debug, Default, PartialEq, Clone)]
// pub struct TupleStructType {
//     pub name: Symbol,
//     pub fields: Vec<ExprType>,
// }

impl ExprType {
    /// Returns true if the type is void.
    pub fn is_void(&self) -> bool {
        matches!(self, ExprType::Void)
    }

    /// Returns true if the type is a tuple.
    pub fn is_integer(&self) -> bool {
        matches!(self, ExprType::I32 | ExprType::I64)
    }

    /// Returns true if the type is a float.
    pub fn is_float(&self) -> bool {
        matches!(self, ExprType::F32 | ExprType::F64)
    }

    /// Returns true if the type is a number.
    pub fn is_number(&self) -> bool {
        self.is_integer() || self.is_float()
    }
}

impl Display for ExprType {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            ExprType::None => write!(f, "None"),
            ExprType::Infer(id) => write!(f, "Infer({})", id.0),
            ExprType::Void => write!(f, "void"),
            ExprType::Boolean => write!(f, "bool"),
            ExprType::IUnsized => write!(f, "{{integer}}"),
            ExprType::I32 => write!(f, "i32"),
            ExprType::I64 => write!(f, "i64"),
            ExprType::F32 => write!(f, "f32"),
            ExprType::F64 => write!(f, "f64"),
            ExprType::String => write!(f, "String"),
            ExprType::Entity => write!(f, "Entity"),
            ExprType::Reflected(type_info) => write!(f, "{}", type_info.type_path()),
            // ExprType::Tuple(types) => {
            //     write!(f, "(")?;
            //     for (i, ty) in types.iter().enumerate() {
            //         if i > 0 {
            //             write!(f, ", ")?;
            //         }
            //         ty.fmt(f)?;
            //     }
            //     write!(f, ")")
            // }
            // ExprType::Array(ty) => write!(f, "[{}]", ty),
            ExprType::Function(ftype) => {
                write!(f, "(")?;
                for (i, param) in ftype.params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    param.typ.fmt(f)?;
                }
                if !matches!(ftype.ret, ExprType::None) {
                    write!(f, ") -> {}", ftype.ret)?
                }
                Ok(())
            } // ExprType::Struct(stype) => {
              //     write!(f, "struct")?;
              //     Ok(())
              // }
              // ExprType::TupleStruct(stype) => {
              //     write!(f, "tuple_struct")?;
              //     Ok(())
              // }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct TypeVarId(pub(crate) usize);

/// Represents a function parameter.
#[derive(Debug, PartialEq)]
pub struct Param {
    pub name: SmolStr,
    pub ty: ExprType,
}
