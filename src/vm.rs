use std::{
    array::from_fn,
    ffi::c_long,
    fmt::Display,
    ops::{Div, Index, IndexMut, Mul, RangeFrom, Sub},
    rc::Rc,
};

use crate::{
    chunk::{
        Chunk, OP_ADD, OP_CALL, OP_CLOSURE, OP_CONSTANT, OP_DEFINE_GLOBAL, OP_DIVIDE, OP_EQUAL,
        OP_FALSE, OP_GET_GLOBAL, OP_GET_LOCAL, OP_GREATER, OP_JUMP, OP_JUMP_IF_FALSE, OP_LESS,
        OP_LOOP, OP_MULTIPLY, OP_NEGATE, OP_NIL, OP_NOT, OP_POP, OP_PRINT, OP_RETURN,
        OP_SET_GLOBAL, OP_SET_LOCAL, OP_SUBTRACT, OP_TRUE,
    },
    compiler::compile,
    debug::disassemble_instruction,
    object::{Intern, NativeFn, ObjClosure, ObjFunction, ObjNative, ObjString, ObjType},
    table::Table,
    value::ValueContent,
};

use crate::value;
use value::Value;

const FRAME_MAX: usize = 64;
const STACK_MAX: usize = FRAME_MAX * u8::MAX as usize;

#[derive(Debug)]
struct CallFrameInfo {
    closure: Rc<ObjClosure>,
    ip: usize,
    value_offset: usize,
}

impl CallFrameInfo {
    fn new(closure: Rc<ObjClosure>, value_offset: usize) -> Self {
        Self {
            closure,
            ip: 0,
            value_offset: value_offset,
        }
    }
}

#[derive(Debug)]
struct CallFrame<'f, 'v> {
    frames: &'f mut FrameStack,
    slots: ValueSlice<'v, STACK_MAX>,
}

impl<'f, 'v> CallFrame<'f, 'v> {
    fn info(&self) -> &CallFrameInfo {
        self.frames.0.last().unwrap()
    }

    fn info_mut(&mut self) -> &mut CallFrameInfo {
        self.frames.0.last_mut().unwrap()
    }

    fn ip(&self) -> usize {
        self.info().ip
    }

    fn ip_mut(&mut self) -> &mut usize {
        &mut self.info_mut().ip
    }

    fn stack(&self) -> &ValueSlice<STACK_MAX> {
        &self.slots
    }

    fn stack_mut(&mut self) -> &mut ValueSlice<'v, STACK_MAX> {
        &mut self.slots
    }

    fn chunk(&self) -> &Chunk {
        &self.info().closure.function.chunk
    }

    fn release(self) -> (&'f mut FrameStack, &'v mut ValueStack<STACK_MAX>) {
        (self.frames, self.slots.stack)
    }
}

#[derive(Debug, Default)]
struct FrameStack(Vec<CallFrameInfo>);

impl FrameStack {
    fn active_frame<'f, 'v>(
        &'f mut self,
        stack: &'v mut ValueStack<STACK_MAX>,
    ) -> CallFrame<'f, 'v> {
        let info = self.0.last().unwrap();
        let offset = info.value_offset;
        CallFrame {
            frames: self,
            slots: ValueSlice::new(stack, offset),
        }
    }
}

#[derive(Debug)]
pub struct VM {
    frames: FrameStack,
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
        let mut res = Self {
            frames: Default::default(),
            stack: Default::default(),
            globals: Default::default(),
            strings: Default::default(),
        };
        let mut stack = ValueSlice::new(&mut res.stack, 0);
        Self::define_native(
            &mut res.globals,
            &mut res.strings,
            &mut stack,
            b"clock",
            clock_native,
        );
        res
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        let script = match compile(source, &mut self.strings) {
            Some(script) => script,
            None => {
                return InterpretResult::CompileError;
            }
        };
        let function: Rc<ObjFunction> = script.into();
        assert_eq!(self.stack.count, 0);
        self.stack.push(Value::from_obj(function.clone()));
        let closure = Rc::new(ObjClosure::new(function));
        self.stack.pop();
        self.stack.push(Value::from_obj(closure.clone()));
        let script = CallFrameInfo::new(closure, 0);
        self.frames.0.push(script);
        self.run()
    }

    fn run(&mut self) -> InterpretResult {
        let mut frame = self.frames.active_frame(&mut self.stack);
        loop {
            #[cfg(feature = "debug_trace_execution")]
            {
                println!("          {}", frame.stack());
                disassemble_instruction(frame.chunk(), frame.ip());
            }
            let instruction = Self::read_byte(&mut frame);
            match instruction {
                OP_CONSTANT => {
                    let constant = Self::read_constant(&mut frame);
                    frame.stack_mut().push(constant);
                }
                OP_NIL => frame.stack_mut().push(Value::Nil),
                OP_TRUE => frame.stack_mut().push(Value::Bool(true)),
                OP_FALSE => frame.stack_mut().push(Value::Bool(false)),
                OP_POP => {
                    frame.stack_mut().pop();
                }
                OP_GET_LOCAL => {
                    let slot = Self::read_byte(&mut frame);
                    let value = frame.stack_mut()[slot as usize].clone();
                    frame.stack_mut().push(value);
                }
                OP_SET_LOCAL => {
                    let slot = Self::read_byte(&mut frame);
                    frame.stack_mut()[slot as usize] = frame.stack_mut().peek(0).unwrap();
                }
                OP_GET_GLOBAL => {
                    let constant = Self::read_constant(&mut frame);
                    let name = constant.as_string_rc();
                    if let Some(value) = self.globals.get(name.clone()) {
                        frame.stack_mut().push(value.clone());
                    } else {
                        Self::runtime_error(&mut frame, &format!("Undefined variable {}", *name));
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_DEFINE_GLOBAL => {
                    let constant = Self::read_constant(&mut frame);
                    let name = constant.as_string_rc();
                    self.globals.set(name, frame.stack_mut().peek(0).unwrap());
                    frame.stack_mut().pop();
                }
                OP_SET_GLOBAL => {
                    let constant = Self::read_constant(&mut frame);
                    let name = constant.as_string_rc();
                    if self
                        .globals
                        .set(name.clone(), frame.stack_mut().peek(0).unwrap())
                        .is_none()
                    {
                        self.globals.delete(name.clone());
                        Self::runtime_error(&mut frame, &format!("Undefined variable {}", *name));
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_EQUAL => {
                    let b = frame.stack_mut().pop();
                    let a = frame.stack_mut().pop();
                    frame.stack_mut().push(Value::Bool(a == b));
                }
                OP_GREATER => {
                    if !Self::binary_op_bool(&mut frame, PartialOrd::gt) {
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_LESS => {
                    if !Self::binary_op_bool(&mut frame, PartialOrd::lt) {
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_ADD => {
                    if frame.stack_mut().peek(0).unwrap().is_string()
                        && frame.stack_mut().peek(1).unwrap().is_string()
                    {
                        VM::concatenate(&mut self.strings, &mut frame);
                    } else if let (Some(Value::Number(right)), Some(Value::Number(left))) =
                        (frame.stack_mut().peek(0), frame.stack_mut().peek(1))
                    {
                        frame.stack_mut().pop();
                        frame.stack_mut().pop();
                        frame.stack_mut().push(ValueContent::to_value(left + right));
                    } else {
                        Self::runtime_error(
                            &mut frame,
                            "Operands must be two numbers or two strings",
                        );
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_SUBTRACT => {
                    if !Self::binary_op_num(&mut frame, Sub::sub) {
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_MULTIPLY => {
                    if !Self::binary_op_num(&mut frame, Mul::mul) {
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_DIVIDE => {
                    if !Self::binary_op_num(&mut frame, Div::div) {
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_NOT => {
                    let value = frame.stack_mut().pop();
                    frame.stack_mut().push(Value::Bool(value.is_falsey()));
                }
                OP_NEGATE => {
                    let value = frame.stack_mut().peek(0);
                    if let Some(Value::Number(number)) = value {
                        frame.stack_mut().push(Value::Number(-number));
                    } else {
                        Self::runtime_error(&mut frame, "Operand must be a number");
                        return InterpretResult::RuntimeError;
                    }
                }
                OP_PRINT => {
                    println!("{}", frame.stack_mut().pop());
                }
                OP_JUMP => {
                    let offset = Self::read_short(&mut frame);
                    *frame.ip_mut() += offset as usize;
                }
                OP_JUMP_IF_FALSE => {
                    let offset = Self::read_short(&mut frame);
                    if frame.stack_mut().peek(0).unwrap().is_falsey() {
                        *frame.ip_mut() += offset as usize;
                    }
                }
                OP_LOOP => {
                    let offset = Self::read_short(&mut frame);
                    *frame.ip_mut() -= offset as usize;
                }
                OP_CALL => {
                    let arg_count = Self::read_byte(&mut frame);
                    let function = frame.stack().peek(arg_count as usize).unwrap();
                    if !Self::call_value(frame, function, arg_count) {
                        return InterpretResult::RuntimeError;
                    }
                    frame = self.frames.active_frame(&mut self.stack);
                }
                OP_CLOSURE => {
                    let function = Self::read_constant(&mut frame).as_concrete_rc();
                    let closure = Rc::new(ObjClosure::new(function));
                    frame.stack_mut().push(Value::from_obj(closure));
                }
                OP_RETURN => {
                    let result = frame.stack_mut().pop();
                    let return_stack_count = frame.info().value_offset;
                    drop(frame);
                    self.frames.0.pop();
                    if self.frames.0.is_empty() {
                        self.stack.pop();
                        return InterpretResult::Ok;
                    }
                    self.stack.count = return_stack_count;
                    self.stack.push(result);
                    frame = self.frames.active_frame(&mut self.stack);
                }
                _ => {}
            }
        }
    }

    fn read_byte(frame: &mut CallFrame) -> u8 {
        let res = frame.chunk()[frame.ip()];
        *frame.ip_mut() += 1;
        res
    }

    fn read_short(frame: &mut CallFrame) -> u16 {
        *frame.ip_mut() += 2;
        (frame.chunk()[frame.ip() - 2] as u16) << 8 | frame.chunk()[frame.ip() - 1] as u16
    }

    fn read_constant(frame: &mut CallFrame) -> Value {
        let index = Self::read_byte(frame) as usize;
        frame.chunk().constants()[index].clone()
    }

    fn runtime_error(frame: &mut CallFrame, msg: &str) {
        eprintln!("{msg}");
        for frame in frame.frames.0.iter().rev() {
            let function = frame.closure.function.clone();
            let instruction = frame.ip - 1;
            let line = function.chunk.lines()[instruction];
            let function_name = match &function.name {
                Some(name) => format!("{name}()"),
                None => "script".to_owned(),
            };
            eprintln!("[line {line}] in {function_name}");
        }

        frame.slots.stack.reset();
    }

    fn define_native<'s>(
        globals: &mut Table,
        intern: &mut impl Intern,
        stack: &mut ValueSlice<'s, STACK_MAX>,
        name: &[u8],
        function: NativeFn,
    ) {
        let name = ObjString::from_u8(intern, name);
        stack.push(Value::from_obj(name));
        stack.push(Value::from_obj(Rc::new(ObjNative::new(function))));
        let key = stack.peek(1).unwrap();
        let key = key.as_string_rc();
        let value = stack.peek(0).unwrap();
        globals.set(key, value);
        stack.pop();
        stack.pop();
    }

    fn binary_op<R, F>(frame: &mut CallFrame, f: F) -> bool
    where
        R: ValueContent,
        F: Fn(f64, f64) -> R,
    {
        if let Some(Value::Number(right)) = frame.stack_mut().peek(0) {
            if let Some(Value::Number(left)) = frame.stack_mut().peek(1) {
                frame.stack_mut().pop();
                frame.stack_mut().pop();
                frame
                    .stack_mut()
                    .push(ValueContent::to_value(f(left, right)));
                return true;
            }
        }
        Self::runtime_error(frame, "Operands must be numbers");
        false
    }

    fn binary_op_num<F>(frame: &mut CallFrame, f: F) -> bool
    where
        F: Fn(f64, f64) -> f64,
    {
        Self::binary_op(frame, f)
    }

    fn binary_op_bool<F>(frame: &mut CallFrame, f: F) -> bool
    where
        F: Fn(&f64, &f64) -> bool,
    {
        Self::binary_op(frame, |a, b| f(&a, &b))
    }

    fn concatenate(intern: &mut impl Intern, frame: &mut CallFrame) {
        let b = frame.stack_mut().pop();
        let a = frame.stack_mut().pop();
        frame
            .stack_mut()
            .push(Value::from_obj(ObjString::concatenate(
                intern,
                &a.as_string(),
                &b.as_string(),
            )));
    }

    fn call_value(mut frame: CallFrame, callee: Value, arg_count: u8) -> bool {
        if let Value::Obj(callee) = callee {
            match callee.kind() {
                ObjType::Closure => {
                    return Self::call(frame, Rc::downcast(callee).unwrap(), arg_count);
                }
                ObjType::Native => {
                    let native = Rc::downcast::<ObjNative>(callee).unwrap();
                    let native = native.native_ptr();
                    let bottom = frame.stack().count() - arg_count as usize;
                    let result = native(arg_count as i32, &frame.stack()[bottom..]);
                    frame.slots.stack.count -= arg_count as usize + 1;
                    frame.stack_mut().push(result);
                    return true;
                }
                _ => todo!(),
            }
        }
        Self::runtime_error(&mut frame, "Can only call functions and classes");
        false
    }

    fn call(mut frame: CallFrame, closure: Rc<ObjClosure>, arg_count: u8) -> bool {
        if arg_count as u32 != closure.function.arity {
            Self::runtime_error(
                &mut frame,
                &format!(
                    "Expected {} arguments but got {}",
                    closure.function.arity, arg_count
                ),
            );
            return false;
        }
        if frame.frames.0.len() == FRAME_MAX {
            Self::runtime_error(&mut frame, "Stack overflow");
            return false;
        }
        let value_offset = frame.slots.stack.count - arg_count as usize - 1;
        let (frames, _values) = frame.release();
        let frame_info = CallFrameInfo::new(closure, value_offset);
        frames.0.push(frame_info);
        true
    }
}

#[link(name = "c")]
extern "C" {
    fn clock() -> c_long;
}

const CLOCKS_PER_SEC: c_long = 1_000_000;

fn clock_native(_arg_count: i32, _args: &[Value]) -> Value {
    let result = unsafe { clock() } as f64 / CLOCKS_PER_SEC as f64;
    return Value::from_number(result);
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

impl<const MAX_SIZE: usize> Index<RangeFrom<usize>> for ValueStack<MAX_SIZE> {
    type Output = [Value];

    fn index(&self, index: RangeFrom<usize>) -> &Self::Output {
        &self.values[index]
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

#[derive(Debug)]
struct ValueSlice<'a, const MAX_SIZE: usize> {
    stack: &'a mut ValueStack<MAX_SIZE>,
    offset: usize,
}

impl<'a, const MAX_SIZE: usize> ValueSlice<'a, MAX_SIZE> {
    fn new(stack: &'a mut ValueStack<MAX_SIZE>, offset: usize) -> Self {
        Self { stack, offset }
    }

    fn count(&self) -> usize {
        self.stack.count - self.offset
    }

    fn values(&self) -> &[Value] {
        &self.stack.values[self.offset..]
    }

    fn values_mut(&mut self) -> &mut [Value] {
        &mut self.stack.values[self.offset..]
    }

    fn peek(&self, distance: usize) -> Option<Value> {
        if self.count() > distance {
            self.stack.peek(distance)
        } else {
            None
        }
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        if self.count() > 0 {
            self.stack.pop()
        } else {
            panic!()
        }
    }
}

impl<'a, const MAX_SIZE: usize> Index<usize> for ValueSlice<'a, MAX_SIZE> {
    type Output = Value;

    fn index(&self, index: usize) -> &Self::Output {
        &self.values()[index]
    }
}

impl<'a, const MAX_SIZE: usize> IndexMut<usize> for ValueSlice<'a, MAX_SIZE> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.values_mut()[index]
    }
}

impl<'a, const MAX_SIZE: usize> Index<RangeFrom<usize>> for ValueSlice<'a, MAX_SIZE> {
    type Output = [Value];

    fn index(&self, index: RangeFrom<usize>) -> &Self::Output {
        &self.values()[index]
    }
}

impl<'a, const MAX_SIZE: usize> Display for ValueSlice<'a, MAX_SIZE> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for value in &self.values()[..self.count()] {
            write!(f, "[ {value} ]")?;
        }
        Ok(())
    }
}
