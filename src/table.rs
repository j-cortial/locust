use std::collections::hash_map::Entry;
use std::hash::Hash;
use std::{collections::HashMap, rc::Rc};

use gc::{Finalize, Trace};

use crate::object::Intern;
use crate::{object::ObjString, value::Value};

#[derive(Debug, Default, Clone, Trace, Finalize)]
pub struct Table(HashMap<Key, Value>);

impl Table {
    pub fn set(&mut self, key: Rc<ObjString>, value: Value) -> Option<Value> {
        self.0.insert(Key(key), value)
    }

    pub fn add(&mut self, other: &Self) {
        self.0
            .extend(other.0.iter().map(|(k, v)| (k.clone(), v.clone())));
    }

    pub fn get(&self, key: Rc<ObjString>) -> Option<&Value> {
        self.0.get(&Key(key))
    }

    pub fn delete(&mut self, key: Rc<ObjString>) -> Option<Value> {
        self.0.remove(&Key(key))
    }
}

#[derive(Debug, Eq, Trace, Finalize)]
struct Key(#[unsafe_ignore_trace] Rc<ObjString>);

impl Key {
    fn underlying(&self) -> &ObjString {
        &self.0
    }
}

impl Clone for Key {
    fn clone(&self) -> Self {
        Self(Rc::clone(&self.0))
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

impl Intern for Table {
    fn intern(&mut self, new_instance: ObjString) -> Rc<ObjString> {
        let candidate = Key(Rc::new(new_instance));
        match self.0.entry(candidate) {
            Entry::Occupied(e) => e.key().clone(),
            Entry::Vacant(e) => {
                let res = e.key().clone();
                e.insert(Value::Nil);
                res
            }
        }
        .0
        .clone()
    }
}
