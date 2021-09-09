use std::rc::Rc;

use crate::ast::{BlockStatement, Identifier};

#[derive(Clone)]
pub enum Object {
    INTEGER(i32),
    BOOLEAN(bool),
    STRING(String),
    RERUTN(Rc<Object>),
    FUNCTION(Vec<Rc<Identifier>>, Rc<BlockStatement>),
    NULL,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::INTEGER(x) => x.to_string(),
            Object::BOOLEAN(x) => x.to_string(),
            Object::STRING(x) => x.to_string(),
            Object::RERUTN(x) => x.inspect(),
            Object::FUNCTION(params, _) => {
                let params_str = params
                    .into_iter()
                    .map(|p| p.token.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("fn({})", params_str)
            }
            Object::NULL => "null".to_string(),
        }
    }
    pub fn object_type(&self) -> String {
        match self {
            Object::INTEGER(_) => "INTEGER".to_string(),
            Object::BOOLEAN(_) => "BOOLEAN".to_string(),
            &Object::STRING(_) => "STRING".to_string(),
            Object::RERUTN(_) => "RERUTN".to_string(),
            Object::NULL => "NULL".to_string(),
            Object::FUNCTION(_, _) => "FUNCTION".to_string(),
        }
    }
}
