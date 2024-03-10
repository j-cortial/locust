use std::{
    fmt::Display,
    ops::{Add, Div, Mul, Sub},
};

use crate::{
    chunk::{
        Chunk, OP_ADD, OP_CONSTANT, OP_DIVIDE, OP_MULTIPLY, OP_NEGATE, OP_RETURN, OP_SUBTRACT,
    },
    debug::disassemble_instruction,
    compiler::compile,
};

use crate::value;
use value::Value;

const STACK_MAX: usize = 256;

pub struct VM {
    chunk: Chunk,
    ip: usize,
    stack: ValueStack<STACK_MAX>,
}

pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

impl VM {
    pub fn new() -> Self {
        Self {
            chunk: Default::default(),
            ip: 0,
            stack: Default::default(),
        }
    }

    pub fn interpret(&mut self, line: &str) -> InterpretResult {
        compile(line);
        InterpretResult::Ok
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            #[cfg(feature = "debug_trace_execution")]
            {
                println!("          {}", self.stack);
                disassemble_instruction(&self.chunk, self.ip);
            }
            let instruction = self.read_byte();
            match instruction {
                OP_CONSTANT => {
                    let index = self.read_byte() as usize;
                    let constant = self.chunk.constants()[index];
                    self.stack.push(constant);
                }
                OP_ADD => {
                    binary_op(&mut self.stack, Add::add);
                }
                OP_SUBTRACT => {
                    binary_op(&mut self.stack, Sub::sub);
                }
                OP_MULTIPLY => {
                    binary_op(&mut self.stack, Mul::mul);
                }
                OP_DIVIDE => {
                    binary_op(&mut self.stack, Div::div);
                }
                OP_NEGATE => {
                    let value = self.stack.pop();
                    self.stack.push(-value);
                }
                OP_RETURN => {
                    println!("{}", self.stack.pop());
                    return InterpretResult::Ok;
                }
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

fn binary_op<const MAX_SIZE: usize, F>(stack: &mut ValueStack<MAX_SIZE>, f: F)
where
    F: Fn(Value, Value) -> Value,
{
    let right = stack.pop();
    let left = stack.pop();
    stack.push(f(left, right));
}
