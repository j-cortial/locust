use std::fmt::Debug;
use std::fmt::Display;
use std::rc::Rc;

extern crate downcast_rs;
use downcast_rs::impl_downcast;
use downcast_rs::Downcast;

use crate::chunk::Chunk;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ObjType {
    Function,
    String,
}

pub trait Obj: Debug + Downcast {
    fn kind(&self) -> ObjType;

    fn as_obj_string(&self) -> Option<&ObjString> {
        None
    }

    fn as_obj_function(&self) -> Option<&ObjFunction> {
        None
    }
}

impl_downcast!(Obj);

impl Display for dyn Obj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind() {
            ObjType::String => write!(f, "{}", self.as_obj_string().unwrap()),
            ObjType::Function => write!(f, "{}", self.as_obj_function().unwrap()),
        }
    }
}

#[derive(Debug)]
pub struct ObjFunction {
    pub arity: u32,
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
        ObjFunction {
            arity: 0,
            chunk: Default::default(),
            name: None,
        }
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
