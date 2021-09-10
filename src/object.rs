use std::rc::Rc;

use crate::ast::{BlockStatement, EvalError, Identifier};

type BuiltInFunc = fn(Vec<Object>) -> Result<Object, EvalError>;

#[derive(Clone)]
pub enum Object {
    INTEGER(i32),
    BOOLEAN(bool),
    STRING(String),
    RERUTN(Rc<Object>),
    FUNCTION(Vec<Rc<Identifier>>, Rc<BlockStatement>),
    ARRAY(Vec<Object>),
    BUILTIN(BuiltInFunc),
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
            Object::ARRAY(elements) => {
                let elements_str = elements
                    .into_iter()
                    .map(|p| p.inspect())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("[{}]", elements_str)
            }
            Object::NULL => "null".to_string(),
            Object::BUILTIN(_) => "builtin".to_string(),
        }
    }
    pub fn object_type(&self) -> String {
        match self {
            Object::INTEGER(_) => "INTEGER".to_string(),
            Object::BOOLEAN(_) => "BOOLEAN".to_string(),
            Object::STRING(_) => "STRING".to_string(),
            Object::RERUTN(_) => "RERUTN".to_string(),
            Object::NULL => "NULL".to_string(),
            Object::FUNCTION(_, _) => "FUNCTION".to_string(),
            Object::ARRAY(_) => "ARRAY".to_string(),
            Object::BUILTIN(_) => "BUILTIN".to_string(),
        }
    }
}
pub fn unwrap_return_value(obj: Object) -> Object {
    match obj {
        Object::RERUTN(x) => (*x).clone(),
        x => x,
    }
}