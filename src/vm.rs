use std::fmt::Display;

use crate::{
    chunk::{self, OP_CONSTANT},
    debug::disassemble_instruction,
};
use chunk::{Chunk, OP_RETURN};

use crate::value;
use value::Value;

const STACK_MAX: usize = 256;

pub struct VM<'a> {
    chunk: &'a Chunk,
    ip: usize,
    stack: ValueStack<STACK_MAX>,
}

pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

impl<'a> VM<'a> {
    pub fn new(chunk: &'a Chunk) -> Self {
        Self {
            chunk,
            ip: 0,
            stack: Default::default(),
        }
    }

    pub fn interpret(&mut self) -> InterpretResult {
        self.run()
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            #[cfg(feature = "debug_trace_execution")]
            {
                println!("          {}", self.stack);
                disassemble_instruction(self.chunk, self.ip);
            }
            let instruction = self.read_byte();
            match instruction {
                OP_CONSTANT => {
                    let constant = self.chunk.constants()[self.read_byte() as usize];
                    self.stack.push(constant);
                }
                OP_RETURN => {
                    println!("{}", self.stack.pop());
                    return InterpretResult::Ok;
                },
                _ => {}
            }
        }
    }

    fn read_byte(&mut self) -> u8 {
        let res = self.chunk[self.ip];
        self.ip += 1;
        res
    }
}

#[derive(Debug)]
struct ValueStack<const MAX_SIZE: usize> {
    values: [Value; MAX_SIZE],
    count: usize,
}

impl<const MAX_SIZE: usize> Default for ValueStack<MAX_SIZE> {
    fn default() -> Self {
        Self {
            values: [Default::default(); MAX_SIZE],
            count: Default::default(),
        }
    }
}

impl<const MAX_SIZE: usize> ValueStack<MAX_SIZE> {
    fn reset(&mut self) {
        self.count = Default::default();
    }

    fn push(&mut self, value: Value) {
        self.values[self.count] = value;
        self.count += 1;
    }

    fn pop(&mut self) -> Value {
        self.count -= 1;
        self.values[self.count]
    }
}

impl<const MAX_SIZE: usize> Display for ValueStack<MAX_SIZE> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for &value in &self.values[..self.count] {
            write!(f, "[ {value} ]")?;
        }
        Ok(())
    }
}
