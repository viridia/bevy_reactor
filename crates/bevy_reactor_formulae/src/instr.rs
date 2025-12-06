// Constants
pub const OP_CONST_VOID: u8 = 2; // Takes up a stack slot but represents no value.
pub const OP_CONST_TRUE: u8 = 3;
pub const OP_CONST_FALSE: u8 = 4;
pub const OP_CONST_I32: u8 = 5; // (imm i32)
pub const OP_CONST_I64: u8 = 6; // (imm i64)
pub const OP_CONST_F32: u8 = 7; // (imm f32)
pub const OP_CONST_F64: u8 = 8; // (imm f64)
pub const OP_CONST_STR: u8 = 9; // (imm u16, [bytes])

// Stack manipulation
pub const OP_DROP: u8 = 10;
pub const OP_DROP_N: u8 = 11; // (imm u32)

// Dereference
pub const OP_LOAD_PARAM: u8 = 20;
pub const OP_LOAD_LOCAL: u8 = 21;
pub const OP_LOAD_GLOBAL: u8 = 22; // (imm u16)
pub const OP_LOAD_ENTITY_PROP: u8 = 23; // (imm u16, consumes TOS)
pub const OP_LOAD_FIELD: u8 = 24; // (imm u16, consumes TOS)
pub const OP_STORE_LOCAL: u8 = 25;

// Binops: all consume TOS + 1 and push result
pub const OP_LOGICAL_AND: u8 = 35;
pub const OP_LOGICAL_OR: u8 = 36;
pub const OP_SHL: u8 = 40;
pub const OP_SHR: u8 = 41;

// Unops: all consume TOS and push result
pub const OP_LOG_NOT: u8 = 50;
pub const OP_NEGATE: u8 = 51;
pub const OP_COMPLEMENT: u8 = 52;

// Control flow
pub const OP_BRANCH: u8 = 62; // (imm i32 relative offset)
pub const OP_BRANCH_IF_FALSE: u8 = 63; // (imm i32 relative offset, consumes TOS)

// Method calls

/// Call a registered method on an entity
/// immediate: [u32, u32]: number of arguments, method index
/// stack input: args
/// stack output: result
pub const OP_CALL: u8 = 70; // Call script function
pub const OP_CALL_ENTITY_METHOD: u8 = 71; // Call method on entity
pub const OP_CALL_HOST_METHOD: u8 = 72; // Call host method
pub const OP_CALL_OBJECT_METHOD: u8 = 73; // Call method of object
pub const OP_RET: u8 = 74; // transfers value from child stack to parent

// Experimental
// Typed Binops: all consume stack[2] and push result
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

pub const OP_EQ_I32: u8 = 150;
pub const OP_EQ_I64: u8 = 151;
pub const OP_EQ_F32: u8 = 152;
pub const OP_EQ_F64: u8 = 152;

pub const OP_NE_I32: u8 = 154;
pub const OP_NE_I64: u8 = 155;
pub const OP_NE_F32: u8 = 156;
pub const OP_NE_F64: u8 = 157;

pub const OP_LT_I32: u8 = 158;
pub const OP_LT_I64: u8 = 159;
pub const OP_LT_F32: u8 = 160;
pub const OP_LT_F64: u8 = 161;

pub const OP_LE_I32: u8 = 162;
pub const OP_LE_I64: u8 = 163;
pub const OP_LE_F32: u8 = 164;
pub const OP_LE_F64: u8 = 165;

pub const OP_GT_I32: u8 = 166;
pub const OP_GT_I64: u8 = 167;
pub const OP_GT_F32: u8 = 168;
pub const OP_GT_F64: u8 = 169;

pub const OP_GE_I32: u8 = 170;
pub const OP_GE_I64: u8 = 171;
pub const OP_GE_F32: u8 = 172;
pub const OP_GE_F64: u8 = 173;

#[derive(Default)]
pub struct InstructionBuilder {
    code: Vec<u8>,
}

impl InstructionBuilder {
    /// Write an instruction opcode (8 bits)
    pub fn push_op(&mut self, op: u8) {
        self.code.push(op);
    }

    /// Write an immediate operand.
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

    /// Write a string as an immediate value.
    pub fn push_immediate_string(&mut self, str: &str) {
        self.code.extend_from_slice(str.as_bytes());
    }

    /// Reserve space for an immediate operand, but don't write anything to it yet.
    /// Return the offset of the reserved value.
    pub fn reserve_immediate<T>(&mut self) -> usize {
        self.align_ip::<T>();
        let slot = std::mem::size_of::<T>();
        let offset = self.code.len();
        self.code.resize(offset + slot, 0);
        offset
    }

    /// The offset of the next available instruction slot.
    pub fn position(&self) -> usize {
        self.code.len()
    }

    /// Write an immediate operand into a previously reserved location. The data type
    /// must match that of the previously reserved space.
    pub fn patch_immediate<T>(&mut self, val: T, offset: usize) {
        let slot = std::mem::size_of::<T>();
        assert!(offset + slot <= self.code.len());
        unsafe {
            let dst = self.code.as_mut_ptr().add(offset) as *mut T;
            std::ptr::write_unaligned(dst, val);
        }
    }

    /// # Arguments
    /// - `from` - the previously reserved immediate operand of the branch instruction
    /// - `to` - the offset which we are jumping to.
    pub fn patch_branch_target(&mut self, from: usize, to: usize) {
        let slot = std::mem::size_of::<i32>();
        assert!(from + slot <= self.code.len());
        unsafe {
            let dst = self.code.as_mut_ptr().add(from) as *mut i32;
            // Note that the jump instruction pre-increments the address before computing
            // the jump, so we need to calculate relative to the end of the immediate value.
            std::ptr::write_unaligned(dst, (to as i32) - ((from + slot) as i32));
        }
    }

    /// Return the generate opcode stream, consuming self.
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

#[derive(Default)]
pub struct InstructionReader<'a> {
    code: &'a [u8],
    offset: usize,
}

impl<'a> InstructionReader<'a> {
    pub fn new(code: &'a [u8]) -> Self {
        Self { code, offset: 0 }
    }

    #[inline(always)]
    pub fn address(&self) -> usize {
        self.offset
    }

    #[inline(always)]
    pub fn at_end(&self) -> bool {
        self.offset >= self.code.len()
    }

    #[inline(always)]
    pub fn next_op(&mut self) -> u8 {
        assert!(self.offset < self.code.len());
        let op = self.code[self.offset];
        self.offset += 1;
        op
    }

    /// Read an immediate value following the current instruction.
    #[inline(always)]
    fn read_immediate<T: Copy>(&mut self) -> T {
        self.align_ip::<T>();
        let val = unsafe { *(self.code.as_ptr().add(self.offset) as *const T) };
        self.offset += std::mem::size_of::<T>();
        val
    }

    #[inline(always)]
    fn align_ip<T>(&mut self) {
        let align = std::mem::align_of::<T>();
        let offset = self.offset;
        self.offset = (offset + (align - 1)) & !(align - 1);
    }
}

/// Print a disassembly (for debugging).
pub fn disassemble(code: &[u8]) {
    let mut reader = InstructionReader::new(code);
    while !reader.at_end() {
        let offset = reader.address();
        let op = reader.next_op();
        eprint!("{offset}: ");
        match op {
            OP_CONST_VOID => {
                eprintln!("void");
            }

            OP_CONST_TRUE => {
                eprintln!("const true");
            }

            OP_CONST_FALSE => {
                eprintln!("const false");
            }

            OP_CONST_I32 => {
                let val = reader.read_immediate::<i32>();
                eprintln!("const i32 {val}");
            }

            OP_CONST_I64 => {
                let val = reader.read_immediate::<i64>();
                eprintln!("const i64 {val}");
            }

            OP_CONST_F32 => {
                let val = reader.read_immediate::<f32>();
                eprintln!("const f32 {val}");
            }

            OP_CONST_F64 => {
                let val = reader.read_immediate::<f64>();
                eprintln!("const f64 {val}");
            }

            OP_CONST_STR => {
                let len = reader.read_immediate::<u16>() as usize;
                let s = &reader.code[reader.offset..reader.offset + len];
                reader.offset += len;
                eprintln!("const str \"{}\"", unsafe { str::from_utf8_unchecked(s) });
            }

            OP_DROP => {
                eprintln!("drop");
            }

            OP_DROP_N => {
                let val = reader.read_immediate::<f64>();
                eprintln!("drop {val}");
            }

            OP_LOAD_PARAM => {
                let val = reader.read_immediate::<u32>();
                eprintln!("load param {val}");
            }

            OP_LOAD_LOCAL => {
                let val = reader.read_immediate::<u16>();
                eprintln!("load local {val}");
            }

            OP_LOAD_GLOBAL => {
                let val = reader.read_immediate::<u16>();
                eprintln!("load global {val}");
            }

            OP_LOAD_ENTITY_PROP => {
                let val = reader.read_immediate::<u16>();
                eprintln!("load entity prop {val}");
            }

            OP_LOAD_FIELD => {
                let val = reader.read_immediate::<u16>();
                eprintln!("load field {val}");
            }

            // pub const OP_LOAD_LOCAL: u8 = 21;
            // pub const OP_LOAD_GLOBAL: u8 = 22; // (imm u32)
            // pub const OP_LOAD_ENTITY_PROP: u8 = 23; // (imm u16, consumes TOS)
            OP_STORE_LOCAL => {
                let val = reader.read_immediate::<u16>();
                eprintln!("store local {val}");
            }

            // // Binops: all consume TOS + 1 and push result
            // pub const OP_LOGICAL_AND: u8 = 35;
            // pub const OP_LOGICAL_OR: u8 = 36;
            // pub const OP_SHL: u8 = 40;
            // pub const OP_SHR: u8 = 41;

            // // Unops: all consume TOS and push result
            // pub const OP_LOG_NOT: u8 = 50;
            // pub const OP_NEGATE: u8 = 51;
            // pub const OP_COMPLEMENT: u8 = 52;

            // // Control flow
            // pub const OP_BRANCH: u8 = 62; // (imm i32 relative offset)
            // pub const OP_BRANCH_IF_FALSE: u8 = 63; // (imm i32 relative offset, consumes TOS)
            OP_CALL => {
                let fn_index = reader.read_immediate::<u32>();
                let num_params = reader.read_immediate::<u16>() as usize;
                eprintln!("call module[{fn_index}] {num_params}",);
            }

            OP_CALL_ENTITY_METHOD => {
                let fn_index = reader.read_immediate::<u32>();
                let num_params = reader.read_immediate::<u16>() as usize;
                eprintln!("call entity[{fn_index}] {num_params}",);
            }

            OP_CALL_OBJECT_METHOD => {
                let fn_index = reader.read_immediate::<u32>();
                let num_params = reader.read_immediate::<u16>() as usize;
                eprintln!("call object[{fn_index}] {num_params}",);
            }

            // pub const OP_CALL_ENTITY_METHOD: u8 = 70; // Call method on entity
            // pub const OP_CALL_HOST_METHOD: u8 = 71; // Call host method

            // // Experimental
            // // Typed Binops: all consume stack[2] and push result
            OP_ADD_I32 => {
                eprintln!("add.i32");
            }

            OP_ADD_I64 => {
                eprintln!("add.i64");
            }

            OP_ADD_F32 => {
                eprintln!("add.f32");
            }

            OP_ADD_F64 => {
                eprintln!("add.f64");
            }

            // pub const OP_SUB_I32: u8 = 104;
            // pub const OP_SUB_I64: u8 = 105;
            // pub const OP_SUB_F32: u8 = 106;
            // pub const OP_SUB_F64: u8 = 107;

            // pub const OP_MUL_I32: u8 = 108;
            // pub const OP_MUL_I64: u8 = 109;
            // pub const OP_MUL_F32: u8 = 110;
            // pub const OP_MUL_F64: u8 = 111;

            // pub const OP_DIV_I32: u8 = 112;
            // pub const OP_DIV_I64: u8 = 113;
            // pub const OP_DIV_F32: u8 = 114;
            // pub const OP_DIV_F64: u8 = 115;

            // pub const OP_REM_I32: u8 = 116;
            // pub const OP_REM_I64: u8 = 117;
            // pub const OP_REM_F32: u8 = 118;
            // pub const OP_REM_F64: u8 = 119;

            // // pub const OP_LOGICAL_AND: u8 = 120;
            // // pub const OP_LOGICAL_OR: u8 = 121;

            // pub const OP_BIT_AND_I32: u8 = 122;
            // pub const OP_BIT_AND_I64: u8 = 123;

            // pub const OP_BIT_OR_I32: u8 = 124;
            // pub const OP_BIT_OR_I64: u8 = 125;

            // pub const OP_BIT_XOR_I32: u8 = 126;
            // pub const OP_BIT_XOR_I64: u8 = 127;

            // // pub const OP_BIT_AND: u8 = 37;
            // // pub const OP_BIT_OR: u8 = 38;
            // // pub const OP_BIT_XOR: u8 = 39;
            // // pub const OP_SHL: u8 = 40;
            // // pub const OP_SHR: u8 = 41;

            // pub const OP_EQ_I32: u8 = 150;
            // pub const OP_EQ_I64: u8 = 151;
            // pub const OP_EQ_F32: u8 = 152;
            // pub const OP_EQ_F64: u8 = 152;

            // pub const OP_NE_I32: u8 = 154;
            // pub const OP_NE_I64: u8 = 155;
            // pub const OP_NE_F32: u8 = 156;
            // pub const OP_NE_F64: u8 = 157;

            // pub const OP_LT_I32: u8 = 158;
            // pub const OP_LT_I64: u8 = 159;
            // pub const OP_LT_F32: u8 = 160;
            // pub const OP_LT_F64: u8 = 161;

            // pub const OP_LE_I32: u8 = 162;
            // pub const OP_LE_I64: u8 = 163;
            // pub const OP_LE_F32: u8 = 164;
            // pub const OP_LE_F64: u8 = 165;

            // pub const OP_GT_I32: u8 = 166;
            // pub const OP_GT_I64: u8 = 167;
            // pub const OP_GT_F32: u8 = 168;
            // pub const OP_GT_F64: u8 = 169;

            // pub const OP_GE_I32: u8 = 170;
            // pub const OP_GE_I64: u8 = 171;
            // pub const OP_GE_F32: u8 = 172;
            // pub const OP_GE_F64: u8 = 173;
            OP_RET => {
                eprintln!("ret");
            }

            _ => {
                eprintln!("invalid instruction {op} (or not implemented in disassembler)");
                break;
            }
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
