use crate::value;
use value::{Value, ValueArray};

use std::ops::Index;

pub enum OpCode {
    OpConstant,
    OpReturn,
}

pub const OP_CONSTANT: u8 = OpCode::OpConstant as u8;
pub const OP_RETURN: u8 = OpCode::OpReturn as u8;

#[derive(Debug, Default)]
pub struct Chunk {
    code: Vec<u8>,
    lines: Vec<i32>,
    constants: ValueArray
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
