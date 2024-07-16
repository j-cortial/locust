use std::hash::Hash;
use std::{collections::HashMap, rc::Rc};

use crate::{object::ObjString, value::Value};

#[derive(Debug, Default, Clone)]
pub struct Table(HashMap<Key, Value>);

#[derive(Debug, Clone, Eq)]
struct Key(Rc<ObjString>);

impl Key {
    fn underlying(&self) -> &ObjString {
        &*self.0
    }
}

impl PartialEq for Key {
    fn eq(&self, other: &Self) -> bool {
        // Use usual string equality
        self.underlying() == other.underlying()
    }
}

impl Hash for Key {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // Use usual string hash
        self.underlying().hash(state);
    }
}
