use core::fmt::Display;
use std::sync::Arc;

use crate::decl::ParamDecl;

/// Represents the type of an expression.
#[derive(Debug, Default, PartialEq, Clone)]
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
    // Struct(Arc<StructType>),
    // TupleStruct(Arc<TupleStructType>),
    // TODO: Struct, Record, Option, Enum
}

/// Type data for a function
#[derive(Debug, Default, PartialEq, Clone)]
pub struct FunctionType {
    pub params: Vec<ParamDecl>,
    pub ret: ExprType,
}

// / Type data for a struct or record
// #[derive(Debug, PartialEq, Default, Clone)]
// pub struct StructType {
//     pub name: Symbol,
//     pub is_record: bool,
//     pub fields: Vec<FieldDecl>,
// }

// / Type data for a tuple struct or record
// #[derive(Debug, Default, PartialEq, Clone)]
// pub struct TupleStructType {
//     pub name: Symbol,
//     pub is_record: bool,
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

    // /// Returns the number of WASM primitives needed to represent the type. This will always
    // /// be 1 for reference types. For tuples, this will be the sum of the number of primitives
    // /// needed to represent each element.
    // pub fn value_count(&self) -> usize {
    //     match self {
    //         ExprType::None | ExprType::Void => 0,
    //         ExprType::Infer(_) => unreachable!("Infer type should be resolved"),
    //         ExprType::Boolean
    //         | ExprType::IUnsized
    //         | ExprType::I32
    //         | ExprType::I64
    //         | ExprType::F32
    //         | ExprType::F64
    //         | ExprType::String => 1,
    //         ExprType::Tuple(types) => types.iter().map(|ty| ty.value_count()).sum(),
    //         ExprType::Array(_) => 1,
    //         ExprType::Function(_) => 1,
    //         ExprType::Struct(stype) => {
    //             if stype.is_record {
    //                 1
    //             } else {
    //                 stype
    //                     .fields
    //                     .iter()
    //                     .map(|field| field.typ.value_count())
    //                     .sum()
    //             }
    //         }
    //         ExprType::TupleStruct(stype) => {
    //             if stype.is_record {
    //                 1
    //             } else {
    //                 stype.fields.iter().map(|field| field.value_count()).sum()
    //             }
    //         }
    //     }
    // }
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
              //     if stype.is_record {
              //         write!(f, "record")?;
              //     } else {
              //         write!(f, "struct")?;
              //     }
              //     Ok(())
              // }
              // ExprType::TupleStruct(stype) => {
              //     if stype.is_record {
              //         write!(f, "tuple_record")?;
              //     } else {
              //         write!(f, "tuple_struct")?;
              //     }
              //     Ok(())
              // }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct TypeVarId(pub(crate) usize);

#[derive(Debug, PartialEq)]
pub struct TypeId(usize);

/// Represents a function parameter.
#[derive(Debug, PartialEq)]
pub struct Param {
    pub name: String,
    pub ty: TypeId,
}
