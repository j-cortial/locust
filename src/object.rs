use std::any::Any;
use std::fmt::Debug;
use std::fmt::Display;
use std::rc::Rc;

use crate::chunk::Chunk;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ObjType {
    Function,
    String,
}

pub trait Obj: Debug + Any {
    fn kind(&self) -> ObjType;
    fn as_obj_string(&self) -> Option<&ObjString> {
        None
    }
    fn as_obj_function(&self) -> Option<&ObjFunction> {
        None
    }
}

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
    arity: u32,
    chunk: Chunk,
    name: Option<Rc<ObjString>>,
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
    pub fn new() -> Rc<Self> {
        Rc::new(ObjFunction {
            arity: 0,
            chunk: Default::default(),
            name: None,
        })
    }
}

impl Display for ObjFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn <{}>", self.name.as_ref().unwrap())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ObjString {
    content: String,
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

    pub fn concatenate(&self, intern: &mut dyn Intern, other: &Self) -> Rc<Self> {
        Self::new(intern, format!("{}{}", self.content, other.content))
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
