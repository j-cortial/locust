use std::{
    fmt::Display,
    ops::{Add, Div, Mul, Sub},
};

use crate::{
    chunk::{
        Chunk, OP_ADD, OP_CONSTANT, OP_DIVIDE, OP_MULTIPLY, OP_NEGATE, OP_RETURN, OP_SUBTRACT,
    },
    compiler::compile,
    debug::disassemble_instruction,
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

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        let mut chunk = Chunk::new();
        if !compile(source, &mut chunk) {
            return InterpretResult::CompileError;
        }
        self.chunk = chunk;
        self.ip = 0;
        self.run()
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
                    if !self.binary_op(Add::add) {
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_SUBTRACT => {
                    if !self.binary_op(Sub::sub) {
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_MULTIPLY => {
                    if !self.binary_op(Mul::mul) {
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_DIVIDE => {
                    if !self.binary_op(Div::div) {
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_NEGATE => {
                    let value = self.stack.peek(0);
                    if let Some(Value::Number(number)) = value {
                        self.stack.push(Value::Number(-number));
                    }
                    self.runtime_error("Operand must be a number");
                    return InterpretResult::RuntimeError;
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

    fn runtime_error(&self, msg: &str) {
        eprintln!("{msg}");
        let instruction = self.ip - 1;
        let line = self.chunk.lines()[instruction];
        eprintln!("[line {line}] in script");
    }

    fn binary_op<F>(&mut self, f: F) -> bool
    where
        F: Fn(f64, f64) -> f64,
    {
        if let Some(Value::Number(right)) = self.stack.peek(0) {
            if let Some(Value::Number(left)) = self.stack.peek(1) {
                self.stack.pop();
                self.stack.pop();
                self.stack.push(Value::Number(f(left, right)));
                return true;
            }
        }
        self.runtime_error("Operands must be numbers");
        false
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

    fn peek(&self, distance: usize) -> Option<Value> {
        if self.count > distance {
            Some(self.values[self.count - (distance + 1)])
        } else {
            None
        }
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
