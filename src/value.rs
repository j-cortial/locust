use std::ops::Deref;

pub type Value = f64;

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
