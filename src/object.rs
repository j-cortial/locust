use std::fmt::Debug;
use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ObjType {
    String,
}

pub trait Obj: Debug {
    fn kind(&self) -> ObjType;
    fn as_obj_string(&self) -> Option<&ObjString> {
        None
    }
}

impl Display for dyn Obj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind() {
            ObjType::String => write!(f, "{}", self.as_obj_string().unwrap())?,
        };
        Ok(())
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
    pub fn from_u8(chars: &[u8]) -> Self {
        Self {
            content: String::from_utf8(chars.to_owned()).unwrap(),
        }
    }

    pub fn length(&self) -> usize {
        self.content.len()
    }

    pub fn chars(&self) -> &str {
        &self.content
    }

    pub fn concatenate(&self, other: &Self) -> Self {
        Self { content: format!("{}{}", self.content, other.content) }
    }
}

impl Display for ObjString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.content)?;
        Ok(())
    }
}
