use std::collections::HashMap;

use crate::object::Object;

pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: HashMap::new(),
        }
    }
    pub fn get(&self, name: &String) -> Option<Object> {
        self.store.get(name).map(|x| x.clone())
    }
    pub fn set(&mut self, name: String, value: Object) {
        self.store.insert(name, value);//.unwrap();
    }
}
