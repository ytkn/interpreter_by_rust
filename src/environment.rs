use std::{collections::HashMap};

use crate::object::Object;

#[derive(Clone)]
pub struct Environment<'a> {
    store: HashMap<String, Object>,
    outer: Option<&'a Environment<'a>>,
}

impl Environment<'_> {
    pub fn new() -> Environment<'static> {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }
    pub fn new_with_outer<'a>(outer: &'a Environment) -> Environment<'a> {
        Environment {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }
    pub fn get(&self, name: &String) -> Option<Object> {
        let got = self.store.get(name).map(|x| x.clone());
        match got {
            Some(obj) => Some(obj),
            None => match self.outer {
                Some(outer) => outer.get(name),
                None => None,
            },
        }
    }
    pub fn set(&mut self, name: String, value: Object) {
        self.store.insert(name, value);
    }
}
