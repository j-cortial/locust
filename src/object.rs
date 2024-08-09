use std::fmt::{Debug, Display};
use std::rc::Rc;

use gc::{Finalize, Gc, GcCell, Trace};

use crate::chunk::Chunk;
use crate::table::Table;
use crate::value::Value;

#[derive(Debug, Clone, Trace, Finalize)]
pub enum Obj {
    Class(Gc<ObjClass>),
    Closure(Gc<ObjClosure>),
    Function(#[unsafe_ignore_trace] Rc<ObjFunction>),
    Instance(Gc<GcCell<ObjInstance>>),
    Native(#[unsafe_ignore_trace] Rc<ObjNative>),
    String(#[unsafe_ignore_trace] Rc<ObjString>),
}

impl Display for Obj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Obj::Class(o) => write!(f, "{}", o.name),
            Obj::Closure(o) => write!(f, "{}", o.function),
            Obj::Function(o) => write!(f, "{}", o),
            Obj::Instance(o) => write!(f, "{} instance", o.borrow().class.name),
            Obj::Native(_) => write!(f, "<native fn>"),
            Obj::String(o) => write!(f, "{}", o),
        }
    }
}

impl Obj {
    fn as_obj_string_rc_ref(&self) -> &Rc<ObjString> {
        match self {
            Self::String(str) => {
                return str;
            }
            _ => {}
        }
        panic!();
    }

    pub fn as_obj_string_rc(&self) -> Rc<ObjString> {
        self.as_obj_string_rc_ref().clone()
    }

    pub fn as_obj_string(&self) -> &ObjString {
        self.as_obj_string_rc_ref()
    }

    fn as_obj_function_rc_ref(&self) -> &Rc<ObjFunction> {
        match self {
            Self::Function(func) => {
                return func;
            }
            _ => {}
        }
        panic!();
    }

    pub fn as_obj_function_rc(&self) -> Rc<ObjFunction> {
        self.as_obj_function_rc_ref().clone()
    }
}

#[derive(Debug, Default)]
pub struct ObjFunction {
    pub arity: u32,
    pub upvalue_count: u32,
    pub chunk: Chunk,
    pub name: Option<Rc<ObjString>>,
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

#[derive(Debug, Trace, Finalize)]
pub enum UpvalueLocation {
    Closed(Value),
    Open(usize),
}

#[derive(Debug, Trace, Finalize)]
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

#[derive(Debug, Trace, Finalize)]
pub struct ObjClosure {
    #[unsafe_ignore_trace]
    pub function: Rc<ObjFunction>,
    pub upvalues: GcCell<Vec<Gc<GcCell<ObjUpvalue>>>>,
}

impl ObjClosure {
    pub fn new(function: Rc<ObjFunction>) -> Self {
        Self {
            function,
            upvalues: Default::default(),
        }
    }
}

#[derive(Debug, Trace, Finalize)]
pub struct ObjClass {
    #[unsafe_ignore_trace]
    name: Rc<ObjString>,
}

impl ObjClass {
    pub fn new(name: Rc<ObjString>) -> Self {
        Self { name }
    }
}

#[derive(Debug, Trace, Finalize)]
pub struct ObjInstance {
    class: Gc<ObjClass>,
    fields: Table,
}

impl ObjInstance {
    pub fn new(class: Gc<ObjClass>) -> Self {
        Self {
            class,
            fields: Default::default(),
        }
    }
}
