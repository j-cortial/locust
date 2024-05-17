use std::ops::Deref;
use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64)
}

impl Default for Value {
    fn default() -> Self {
        Self::Nil
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(n) => Ok(write!(f, "{n}")?),
            _ => Err(Default::default()),
        }
    }
}

#[derive(Debug, Default)]
pub struct ValueArray {
    values: Vec<Value>
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
