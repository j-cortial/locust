use std::{
    array::from_fn,
    fmt::Display,
    ops::{Div, Index, IndexMut, Mul, Sub},
};

use crate::{
    chunk::{
        Chunk, OP_ADD, OP_CONSTANT, OP_DEFINE_GLOBAL, OP_DIVIDE, OP_EQUAL, OP_FALSE, OP_GET_GLOBAL,
        OP_GET_LOCAL, OP_GREATER, OP_JUMP, OP_JUMP_IF_FALSE, OP_LESS, OP_LOOP, OP_MULTIPLY,
        OP_NEGATE, OP_NIL, OP_NOT, OP_POP, OP_PRINT, OP_RETURN, OP_SET_GLOBAL, OP_SET_LOCAL,
        OP_SUBTRACT, OP_TRUE,
    },
    compiler::compile,
    debug::disassemble_instruction,
    object::ObjString,
    table::Table,
    value::ValueContent,
};

use crate::value;
use value::Value;

const STACK_MAX: usize = 256;

#[derive(Debug)]
pub struct VM {
    ip: usize,
    stack: ValueStack<STACK_MAX>,
    globals: Table,
    strings: Table,
}

#[derive(Debug, Clone, Copy)]
pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

impl VM {
    pub fn new() -> Self {
        Self {
            ip: Default::default(),
            stack: Default::default(),
            globals: Default::default(),
            strings: Default::default(),
        }
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        let script = match compile(source, &mut self.strings) {
            Some(script) => script,
            None => {
                return InterpretResult::CompileError;
            }
        };
        self.ip = 0;
        self.run(&script.chunk)
    }

    fn run(&mut self, chunk: &Chunk) -> InterpretResult {
        loop {
            #[cfg(feature = "debug_trace_execution")]
            {
                println!("          {}", self.stack);
                disassemble_instruction(chunk, self.ip);
            }
            let instruction = self.read_byte(chunk);
            match instruction {
                OP_CONSTANT => {
                    let constant = self.read_constant(chunk);
                    self.stack.push(constant);
                }
                OP_NIL => self.stack.push(Value::Nil),
                OP_TRUE => self.stack.push(Value::Bool(true)),
                OP_FALSE => self.stack.push(Value::Bool(false)),
                OP_POP => {
                    self.stack.pop();
                }
                OP_GET_LOCAL => {
                    let slot = self.read_byte(chunk);
                    self.stack.push(self.stack[slot as usize].clone());
                }
                OP_SET_LOCAL => {
                    let slot = self.read_byte(chunk);
                    self.stack[slot as usize] = self.stack.peek(0).unwrap();
                }
                OP_GET_GLOBAL => {
                    let constant = self.read_constant(chunk);
                    let name = constant.as_string_rc();
                    if let Some(value) = self.globals.get(name) {
                        self.stack.push(value.clone());
                    } else {
                        self.runtime_error(chunk, "Undefined variable {&*name}");
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_DEFINE_GLOBAL => {
                    let constant = self.read_constant(chunk);
                    let name = constant.as_string_rc();
                    self.globals.set(name, self.stack.peek(0).unwrap());
                    self.stack.pop();
                }
                OP_SET_GLOBAL => {
                    let constant = self.read_constant(chunk);
                    let name = constant.as_string_rc();
                    if self
                        .globals
                        .set(name.clone(), self.stack.peek(0).unwrap())
                        .is_none()
                    {
                        self.globals.delete(name);
                        self.runtime_error(chunk, "Undefined variable {&*name}");
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_EQUAL => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.stack.push(Value::Bool(a == b));
                }
                OP_GREATER => {
                    if !self.binary_op_bool(chunk, PartialOrd::gt) {
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_LESS => {
                    if !self.binary_op_bool(chunk, PartialOrd::lt) {
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_ADD => {
                    if self.stack.peek(0).unwrap().is_string()
                        && self.stack.peek(1).unwrap().is_string()
                    {
                        self.concatenate();
                    } else if let (Some(Value::Number(right)), Some(Value::Number(left))) =
                        (self.stack.peek(0), self.stack.peek(1))
                    {
                        self.stack.pop();
                        self.stack.pop();
                        self.stack.push(ValueContent::to_value(left + right));
                    } else {
                        self.runtime_error(chunk, "Operands must be two numbers or two strings");
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_SUBTRACT => {
                    if !self.binary_op_num(chunk, Sub::sub) {
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_MULTIPLY => {
                    if !self.binary_op_num(chunk, Mul::mul) {
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_DIVIDE => {
                    if !self.binary_op_num(chunk, Div::div) {
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
                        self.runtime_error(chunk, "Operand must be a number");
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_PRINT => {
                    println!("{}", self.stack.pop());
                }
                OP_JUMP => {
                    let offset = self.read_short(chunk);
                    self.ip += offset as usize;
                }
                OP_JUMP_IF_FALSE => {
                    let offset = self.read_short(chunk);
                    if self.stack.peek(0).unwrap().is_falsey() {
                        self.ip += offset as usize;
                    }
                }
                OP_LOOP => {
                    let offset = self.read_short(chunk);
                    self.ip -= offset as usize;
                }
                OP_RETURN => {
                    // Exit interpreter
                    return InterpretResult::Ok;
                }
                _ => {}
            }
        }
    }

    fn read_byte(&mut self, chunk: &Chunk) -> u8 {
        let res = chunk[self.ip];
        self.ip += 1;
        res
    }

    fn read_short(&mut self, chunk: &Chunk) -> u16 {
        self.ip += 2;
        (chunk[self.ip - 2] as u16) << 8 | chunk[self.ip - 1] as u16
    }

    fn read_constant(&mut self, chunk: &Chunk) -> Value {
        let index = self.read_byte(chunk) as usize;
        chunk.constants()[index].clone()
    }

    fn runtime_error(&self, chunk: &Chunk, msg: &str) {
        eprintln!("{msg}");
        let instruction = self.ip - 1;
        let line = chunk.lines()[instruction];
        eprintln!("[line {line}] in script");
    }

    fn binary_op<R, F>(&mut self, chunk: &Chunk, f: F) -> bool
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
        self.runtime_error(chunk, "Operands must be numbers");
        false
    }

    fn binary_op_num<F>(&mut self, chunk: &Chunk, f: F) -> bool
    where
        F: Fn(f64, f64) -> f64,
    {
        self.binary_op(chunk, f)
    }

    fn binary_op_bool<F>(&mut self, chunk: &Chunk, f: F) -> bool
    where
        F: Fn(&f64, &f64) -> bool,
    {
        self.binary_op(chunk, |a, b| f(&a, &b))
    }

    fn concatenate(&mut self) {
        let b = self.stack.pop();
        let a = self.stack.pop();
        self.stack.push(Value::from_obj(ObjString::concatenate(
            &mut self.strings,
            &a.as_string(),
            &b.as_string(),
        )));
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
