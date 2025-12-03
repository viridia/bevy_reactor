// Constants
pub const OP_CONST_TRUE: u8 = 1;
pub const OP_CONST_FALSE: u8 = 2;
pub const OP_CONST_I32: u8 = 3; // (imm i32)
pub const OP_CONST_I64: u8 = 4; // (imm i64)
pub const OP_CONST_F32: u8 = 5; // (imm f32)
pub const OP_CONST_F64: u8 = 6; // (imm f64)

// Stack manipulation
pub const OP_DROP: u8 = 10;
pub const OP_DROP_N: u8 = 11; // (imm u32)

// Dereference
pub const OP_LOAD_PARAM: u8 = 20;
pub const OP_LOAD_GLOBAL: u8 = 21; // (imm u32)
pub const OP_LOAD_ENTITY_PROP: u8 = 22; // (imm u32, consumes TOS)

// Binops: all consume TOS + 1 and push result
pub const OP_LOGICAL_AND: u8 = 35;
pub const OP_LOGICAL_OR: u8 = 36;
pub const OP_SHL: u8 = 40;
pub const OP_SHR: u8 = 41;
pub const OP_EQUAL: u8 = 42;
pub const OP_NOT_EQUAL: u8 = 43;
pub const OP_LESS: u8 = 44;
pub const OP_LESS_EQUAL: u8 = 45;
pub const OP_GREATER: u8 = 46;
pub const OP_GREATER_EQUAL: u8 = 47;

// Unops: all consume TOS and push result
pub const OP_LOG_NOT: u8 = 50;
pub const OP_NEGATE: u8 = 51;
pub const OP_COMPLEMENT: u8 = 52;

// Control flow
pub const OP_RET: u8 = 61;
pub const OP_BRANCH: u8 = 62; // (imm i32 relative offset)
pub const OP_BRANCH_IF_TRUE: u8 = 62; // (imm i32 relative offset, consumes TOS)

// Method calls

/// Call a registered method on an entity
/// immediate: [i32, usize]: number of arguments, method index
/// stack input: args
/// stack output: result
pub const OP_CALL_ENTITY_METHOD: u8 = 70;

// Experimental
// Typed Binops: all consume TOS + 1 and push result
pub const OP_ADD_I32: u8 = 100;
pub const OP_ADD_I64: u8 = 101;
pub const OP_ADD_F32: u8 = 102;
pub const OP_ADD_F64: u8 = 103;

pub const OP_SUB_I32: u8 = 104;
pub const OP_SUB_I64: u8 = 105;
pub const OP_SUB_F32: u8 = 106;
pub const OP_SUB_F64: u8 = 107;

pub const OP_MUL_I32: u8 = 108;
pub const OP_MUL_I64: u8 = 109;
pub const OP_MUL_F32: u8 = 110;
pub const OP_MUL_F64: u8 = 111;

pub const OP_DIV_I32: u8 = 112;
pub const OP_DIV_I64: u8 = 113;
pub const OP_DIV_F32: u8 = 114;
pub const OP_DIV_F64: u8 = 115;

pub const OP_REM_I32: u8 = 116;
pub const OP_REM_I64: u8 = 117;
pub const OP_REM_F32: u8 = 118;
pub const OP_REM_F64: u8 = 119;

// pub const OP_LOGICAL_AND: u8 = 120;
// pub const OP_LOGICAL_OR: u8 = 121;

pub const OP_BIT_AND_I32: u8 = 122;
pub const OP_BIT_AND_I64: u8 = 123;

pub const OP_BIT_OR_I32: u8 = 124;
pub const OP_BIT_OR_I64: u8 = 125;

pub const OP_BIT_XOR_I32: u8 = 126;
pub const OP_BIT_XOR_I64: u8 = 127;

// pub const OP_BIT_AND: u8 = 37;
// pub const OP_BIT_OR: u8 = 38;
// pub const OP_BIT_XOR: u8 = 39;
// pub const OP_SHL: u8 = 40;
// pub const OP_SHR: u8 = 41;
// pub const OP_EQUAL: u8 = 42;
// pub const OP_NOT_EQUAL: u8 = 43;
// pub const OP_LESS: u8 = 44;
// pub const OP_LESS_EQUAL: u8 = 45;
// pub const OP_GREATER: u8 = 46;
// pub const OP_GREATER_EQUAL: u8 = 47;

#[derive(Default)]
pub struct InstructionBuilder {
    code: Vec<u8>,
}

impl InstructionBuilder {
    pub fn push_op(&mut self, op: u8) {
        self.code.push(op);
    }

    pub fn push_immediate<T>(&mut self, val: T) {
        self.align_ip::<T>();
        let slot = std::mem::size_of::<T>();
        let offset = self.code.len();
        self.code.resize(offset + slot, 0);
        unsafe {
            let dst = self.code.as_mut_ptr().add(offset) as *mut T;
            std::ptr::write_unaligned(dst, val);
        }
    }

    pub fn inner(self) -> Vec<u8> {
        self.code
    }

    #[inline(always)]
    fn align_ip<T>(&mut self) {
        let align = std::mem::align_of::<T>();
        let mut len = self.code.len();
        let aligned_addr = (len + (align - 1)) & !(align - 1);
        while len < aligned_addr {
            self.code.push(0);
            len += 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_build_instrs() {
        let mut builder = InstructionBuilder::default();
        builder.push_op(OP_CONST_TRUE);
        builder.push_op(OP_CONST_FALSE);
        builder.push_op(OP_ADD_I32);
        assert_eq!(
            builder.code,
            vec![OP_CONST_TRUE, OP_CONST_FALSE, OP_ADD_I32]
        );
    }

    #[test]
    fn test_build_imm() {
        let mut builder = InstructionBuilder::default();
        builder.push_op(OP_CONST_I32);
        builder.push_immediate(10i32);
        assert_eq!(builder.code.len(), 8);
    }
}
