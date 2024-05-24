use std::fmt::Display;
use std::ops::Deref;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
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
        })
    }
}

impl Value {
    pub fn is_falsey(&self) -> bool {
        match self {
            Value::Bool(b) => !*b,
            Value::Nil => true,
            Value::Number(_) => false,
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

#[derive(Debug, Default)]
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
    pub fn new() -> Self {
        Self::default()
    }

    pub fn write(&mut self, value: Value) {
        self.values.push(value)
    }

    pub fn count(&self) -> usize {
        self.values.len()
    }
}

pub fn print_value(value: Value) {
    print!("{value}");
}
