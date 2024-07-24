use crate::value;
use value::{Value, ValueArray};

use std::ops::{Index, IndexMut};

pub enum OpCode {
    OpConstant,
    OpNil,
    OpTrue,
    OpFalse,
    OpPop,
    OpGetLocal,
    OpSetLocal,
    OpGetGlobal,
    OpDefineGlobal,
    OpSetGlobal,
    OpEqual,
    OpGreater,
    OpLess,
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,
    OpNot,
    OpNegate,
    OpPrint,
    OpJump,
    OpJumpIfFalse,
    OpLoop,
    OpReturn,
}

pub const OP_CONSTANT: u8 = OpCode::OpConstant as u8;
pub const OP_NIL: u8 = OpCode::OpNil as u8;
pub const OP_TRUE: u8 = OpCode::OpTrue as u8;
pub const OP_FALSE: u8 = OpCode::OpFalse as u8;
pub const OP_POP: u8 = OpCode::OpPop as u8;
pub const OP_GET_LOCAL: u8 = OpCode::OpGetLocal as u8;
pub const OP_SET_LOCAL: u8 = OpCode::OpSetLocal as u8;
pub const OP_GET_GLOBAL: u8 = OpCode::OpGetGlobal as u8;
pub const OP_DEFINE_GLOBAL: u8 = OpCode::OpDefineGlobal as u8;
pub const OP_SET_GLOBAL: u8 = OpCode::OpSetGlobal as u8;
pub const OP_EQUAL: u8 = OpCode::OpEqual as u8;
pub const OP_GREATER: u8 = OpCode::OpGreater as u8;
pub const OP_LESS: u8 = OpCode::OpLess as u8;
pub const OP_ADD: u8 = OpCode::OpAdd as u8;
pub const OP_SUBTRACT: u8 = OpCode::OpSubtract as u8;
pub const OP_MULTIPLY: u8 = OpCode::OpMultiply as u8;
pub const OP_DIVIDE: u8 = OpCode::OpDivide as u8;
pub const OP_NOT: u8 = OpCode::OpNot as u8;
pub const OP_NEGATE: u8 = OpCode::OpNegate as u8;
pub const OP_PRINT: u8 = OpCode::OpPrint as u8;
pub const OP_JUMP: u8 = OpCode::OpJump as u8;
pub const OP_JUMP_IF_FALSE: u8 = OpCode::OpJumpIfFalse as u8;
pub const OP_LOOP: u8 = OpCode::OpLoop as u8;
pub const OP_RETURN: u8 = OpCode::OpReturn as u8;

#[derive(Debug, Default)]
pub struct Chunk {
    code: Vec<u8>,
    lines: Vec<i32>,
    constants: ValueArray,
}

impl Chunk {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn count(&self) -> usize {
        self.code.len()
    }

    pub fn write(&mut self, byte: u8, line: i32) {
        self.code.push(byte);
        self.lines.push(line);
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.write(value);
        self.constants.count() - 1
    }

    pub fn constants(&self) -> &[Value] {
        &self.constants
    }

    pub fn lines(&self) -> &[i32] {
        &self.lines
    }

    pub fn iter(&self) -> impl Iterator<Item = &u8> {
        self.code.iter()
    }
}

impl Index<usize> for Chunk {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        &self.code[index]
    }
}

impl IndexMut<usize> for Chunk {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.code[index]
    }
}
