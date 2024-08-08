use std::ops::Deref;
use std::{fmt::Display, rc::Rc};

use crate::object::{Obj, ObjFunction, ObjString};

#[derive(Debug, Clone)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    Obj(Obj),
}

impl Default for Value {
    fn default() -> Self {
        Self::nil()
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(match self {
            Self::Bool(b) => {
                if *b {
                    write!(f, "true")?
                } else {
                    write!(f, "false")?
                }
            }
            Self::Number(n) => write!(f, "{n}")?,
            Self::Nil => write!(f, "nil")?,
            Self::Obj(o) => write!(f, "{}", o)?,
        })
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::Obj(l0), Self::Obj(r0)) => {
                let l0 = l0.as_obj_string();
                let r0 = r0.as_obj_string();
                // Exploit systematic string interning
                (l0 as *const ObjString) == (r0 as *const ObjString)
            }
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl Value {
    pub fn is_falsey(&self) -> bool {
        match self {
            Value::Bool(b) => !*b,
            Value::Nil => true,
            _ => false,
        }
    }

    pub fn nil() -> Self {
        Self::Nil
    }

    pub fn from_bool(b: bool) -> Self {
        Self::Bool(b)
    }

    pub fn from_number(n: f64) -> Self {
        Self::Number(n)
    }

    pub fn from_obj(obj: Obj) -> Self {
        Self::Obj(obj)
    }

    pub fn is_string(&self) -> bool {
        match self {
            Self::Obj(obj) => match obj {
                Obj::String(_) => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn as_obj(&self) -> &Obj {
        if let Self::Obj(obj) = self {
            return obj;
        }
        panic!()
    }

    pub fn as_string(&self) -> &ObjString {
        self.as_obj().as_obj_string()
    }

    pub fn as_string_rc(&self) -> Rc<ObjString> {
        self.as_obj().as_obj_string_rc()
    }

    pub fn as_function_rc(&self) -> Rc<ObjFunction> {
        self.as_obj().as_obj_function_rc()
    }
}

pub trait ValueContent {
    fn to_value(self) -> Value;
}

impl ValueContent for bool {
    fn to_value(self) -> Value {
        Value::from_bool(self)
    }
}

impl ValueContent for f64 {
    fn to_value(self) -> Value {
        Value::from_number(self)
    }
}

#[derive(Debug, Default, Clone)]
pub struct ValueArray {
    values: Vec<Value>,
}

impl Deref for ValueArray {
    type Target = [Value];

    fn deref(&self) -> &Self::Target {
        &self.values
    }
}

impl ValueArray {
    pub fn write(&mut self, value: Value) {
        self.values.push(value)
    }

    pub fn count(&self) -> usize {
        self.values.len()
    }
}
