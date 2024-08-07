use std::cell::RefCell;
use std::fmt::Debug;
use std::fmt::Display;
use std::rc::Rc;

extern crate downcast_rs;
use downcast_rs::impl_downcast;
use downcast_rs::Downcast;

use crate::chunk::Chunk;
use crate::value::Value;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ObjType {
    Closure,
    Function,
    Native,
    String,
    Upvalue,
}

pub trait Obj: Debug + Downcast {
    fn kind(&self) -> ObjType;

    fn as_obj_string(&self) -> Option<&ObjString> {
        None
    }

    fn as_obj_function(&self) -> Option<&ObjFunction> {
        None
    }

    fn as_obj_native(&self) -> Option<&ObjNative> {
        None
    }

    fn as_obj_closure(&self) -> Option<&ObjClosure> {
        None
    }
}

impl_downcast!(Obj);

impl Display for dyn Obj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind() {
            ObjType::String => write!(f, "{}", self.as_obj_string().unwrap()),
            ObjType::Function => write!(f, "{}", self.as_obj_function().unwrap()),
            ObjType::Native => write!(f, "<native fn>"),
            ObjType::Closure => write!(f, "{}", self.as_obj_closure().unwrap().function),
            ObjType::Upvalue => write!(f, "upvalue"),
        }
    }
}

#[derive(Debug, Default)]
pub struct ObjFunction {
    pub arity: u32,
    pub upvalue_count: u32,
    pub chunk: Chunk,
    pub name: Option<Rc<ObjString>>,
}

impl Obj for ObjFunction {
    fn kind(&self) -> ObjType {
        ObjType::Function
    }

    fn as_obj_function(&self) -> Option<&ObjFunction> {
        Some(self)
    }
}

impl ObjFunction {
    pub fn new() -> Self {
        ObjFunction::default()
    }
}

impl Display for ObjFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.name.as_ref() {
            Some(n) => write!(f, "fn <{}>", n),
            None => write!(f, "<script>"),
        }
    }
}

pub type NativeFn = fn(arg_count: i32, args: &[Value]) -> Value;

#[derive(Debug)]
pub struct ObjNative {
    function: NativeFn,
}

impl Obj for ObjNative {
    fn kind(&self) -> ObjType {
        ObjType::Native
    }

    fn as_obj_native(&self) -> Option<&ObjNative> {
        Some(self)
    }
}

impl ObjNative {
    pub fn new(function: NativeFn) -> Self {
        Self { function }
    }

    pub fn native_ptr(&self) -> NativeFn {
        self.function
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ObjString {
    pub content: String,
}

impl Obj for ObjString {
    fn kind(&self) -> ObjType {
        ObjType::String
    }

    fn as_obj_string(&self) -> Option<&ObjString> {
        Some(self)
    }
}

impl ObjString {
    fn new(intern: &mut dyn Intern, content: String) -> Rc<Self> {
        intern.intern(Self { content })
    }

    pub fn from_u8(intern: &mut dyn Intern, chars: &[u8]) -> Rc<Self> {
        Self::new(intern, String::from_utf8(chars.to_owned()).unwrap())
    }

    pub fn concatenate(intern: &mut dyn Intern, first: &Self, second: &Self) -> Rc<Self> {
        Self::new(intern, format!("{}{}", first.content, second.content))
    }
}

impl Display for ObjString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.content)
    }
}

pub trait Intern {
    fn intern(&mut self, new_instance: ObjString) -> Rc<ObjString>;
}

#[derive(Debug)]
pub enum UpvalueLocation {
    Open(usize),
    Closed(Value),
}

#[derive(Debug)]
pub struct ObjUpvalue {
    pub location: UpvalueLocation,
}

impl ObjUpvalue {
    pub fn new(index: usize) -> Self {
        Self {
            location: UpvalueLocation::Open(index),
        }
    }

    pub fn location_ref<'a, 's: 'a>(&'a self, stack: &'s [Value]) -> &'a Value {
        match &self.location {
            UpvalueLocation::Open(index) => &stack[*index as usize],
            UpvalueLocation::Closed(value) => &value,
        }
    }

    pub fn location_mut<'a, 's: 'a>(&'a mut self, stack: &'s mut [Value]) -> &'a mut Value {
        match &mut self.location {
            UpvalueLocation::Open(index) => &mut stack[*index as usize],
            UpvalueLocation::Closed(ref mut value) => value,
        }
    }
}

impl Obj for ObjUpvalue {
    fn kind(&self) -> ObjType {
        ObjType::Upvalue
    }
}

#[derive(Debug)]
pub struct ObjClosure {
    pub function: Rc<ObjFunction>,
    pub upvalues: RefCell<Vec<Rc<RefCell<ObjUpvalue>>>>,
}

impl ObjClosure {
    pub fn new(function: Rc<ObjFunction>) -> Self {
        Self {
            function,
            upvalues: Default::default(),
        }
    }
}

impl Obj for ObjClosure {
    fn kind(&self) -> ObjType {
        ObjType::Closure
    }

    fn as_obj_closure(&self) -> Option<&ObjClosure> {
        Some(&self)
    }
}
