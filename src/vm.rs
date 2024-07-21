use std::{
    array::from_fn,
    fmt::Display,
    ops::{Div, Index, IndexMut, Mul, Sub},
};

use crate::{
    chunk::{
        Chunk, OP_ADD, OP_CONSTANT, OP_DEFINE_GLOBAL, OP_DIVIDE, OP_EQUAL, OP_FALSE, OP_GET_GLOBAL,
        OP_GET_LOCAL, OP_GREATER, OP_LESS, OP_MULTIPLY, OP_NEGATE, OP_NIL, OP_NOT, OP_POP,
        OP_PRINT, OP_RETURN, OP_SET_GLOBAL, OP_SET_LOCAL, OP_SUBTRACT, OP_TRUE,
    },
    compiler::compile,
    debug::disassemble_instruction,
    table::Table,
    value::ValueContent,
};

use crate::value;
use value::Value;

const STACK_MAX: usize = 256;

pub struct VM {
    chunk: Chunk,
    ip: usize,
    stack: ValueStack<STACK_MAX>,
    globals: Table,
    strings: Table,
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
            globals: Default::default(),
            strings: Default::default(),
        }
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        let mut chunk = Chunk::new();
        if compile(source, &mut chunk, &mut self.strings) {
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
                    let constant = self.read_constant();
                    self.stack.push(constant);
                }
                OP_NIL => self.stack.push(Value::Nil),
                OP_TRUE => self.stack.push(Value::Bool(true)),
                OP_FALSE => self.stack.push(Value::Bool(false)),
                OP_POP => {
                    self.stack.pop();
                }
                OP_GET_LOCAL => {
                    let slot = self.read_byte();
                    self.stack.push(self.stack[slot as usize].clone());
                }
                OP_SET_LOCAL => {
                    let slot = self.read_byte();
                    self.stack[slot as usize] = self.stack.peek(0).unwrap();
                }
                OP_GET_GLOBAL => {
                    let constant = self.read_constant();
                    let name = constant.as_string_rc();
                    if let Some(value) = self.globals.get(name) {
                        self.stack.push(value.clone());
                    } else {
                        self.runtime_error("Undefined variable {&*name}");
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_DEFINE_GLOBAL => {
                    let constant = self.read_constant();
                    let name = constant.as_string_rc();
                    self.globals.set(name, self.stack.peek(0).unwrap());
                    self.stack.pop();
                }
                OP_SET_GLOBAL => {
                    let constant = self.read_constant();
                    let name = constant.as_string_rc();
                    if self
                        .globals
                        .set(name.clone(), self.stack.peek(0).unwrap())
                        .is_none()
                    {
                        self.globals.delete(name);
                        self.runtime_error("Undefined variable {&*name}");
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_EQUAL => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.stack.push(Value::Bool(a == b));
                }
                OP_GREATER => {
                    if !self.binary_op_bool(PartialOrd::gt) {
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_LESS => {
                    if !self.binary_op_bool(PartialOrd::lt) {
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_ADD => {
                    if self.stack.peek(0).unwrap().is_string()
                        && self.stack.peek(1).unwrap().is_string()
                    {
                        self.concatenate();
                    } else if let Some(Value::Number(right)) = self.stack.peek(0) {
                        if let Some(Value::Number(left)) = self.stack.peek(1) {
                            self.stack.pop();
                            self.stack.pop();
                            self.stack.push(ValueContent::to_value(left + right));
                        } else {
                            self.runtime_error("Operands must be two numbers or two strings");
                            return InterpretResult::RuntimeError;
                        }
                    }
                }
                OP_SUBTRACT => {
                    if !self.binary_op_num(Sub::sub) {
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_MULTIPLY => {
                    if !self.binary_op_num(Mul::mul) {
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_DIVIDE => {
                    if !self.binary_op_num(Div::div) {
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_NOT => {
                    let value = self.stack.pop();
                    self.stack.push(Value::Bool(value.is_falsey()));
                }
                OP_NEGATE => {
                    let value = self.stack.peek(0);
                    if let Some(Value::Number(number)) = value {
                        self.stack.push(Value::Number(-number));
                    } else {
                        self.runtime_error("Operand must be a number");
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_PRINT => {
                    println!("{}", self.stack.pop());
                }
                OP_RETURN => {
                    // Exit interpreter
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

    fn read_constant(&mut self) -> Value {
        let index = self.read_byte() as usize;
        self.chunk.constants()[index].clone()
    }

    fn runtime_error(&self, msg: &str) {
        eprintln!("{msg}");
        let instruction = self.ip - 1;
        let line = self.chunk.lines()[instruction];
        eprintln!("[line {line}] in script");
    }

    fn binary_op<R, F>(&mut self, f: F) -> bool
    where
        R: ValueContent,
        F: Fn(f64, f64) -> R,
    {
        if let Some(Value::Number(right)) = self.stack.peek(0) {
            if let Some(Value::Number(left)) = self.stack.peek(1) {
                self.stack.pop();
                self.stack.pop();
                self.stack.push(ValueContent::to_value(f(left, right)));
                return true;
            }
        }
        self.runtime_error("Operands must be numbers");
        false
    }

    fn binary_op_num<F>(&mut self, f: F) -> bool
    where
        F: Fn(f64, f64) -> f64,
    {
        self.binary_op(f)
    }

    fn binary_op_bool<F>(&mut self, f: F) -> bool
    where
        F: Fn(&f64, &f64) -> bool,
    {
        self.binary_op(|a, b| f(&a, &b))
    }

    fn concatenate(&mut self) {
        let b = self.stack.pop();
        let a = self.stack.pop();
        self.stack.push(Value::from_obj(
            a.as_string().concatenate(&mut self.strings, b.as_string()),
        ));
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
            values: from_fn(|_| Default::default()),
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
            Some(self.values[self.count - (distance + 1)].clone())
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
        self.values[self.count].clone()
    }
}

impl<const MAX_SIZE: usize> Index<usize> for ValueStack<MAX_SIZE> {
    type Output = Value;

    fn index(&self, index: usize) -> &Self::Output {
        &self.values[index]
    }
}

impl<const MAX_SIZE: usize> IndexMut<usize> for ValueStack<MAX_SIZE> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.values[index]
    }
}

impl<const MAX_SIZE: usize> Display for ValueStack<MAX_SIZE> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for value in &self.values[..self.count] {
            write!(f, "[ {value} ]")?;
        }
        Ok(())
    }
}
