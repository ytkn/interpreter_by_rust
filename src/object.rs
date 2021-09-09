use std::rc::Rc;

use crate::ast::{BlockStatement, EvalError, Identifier};

type BuiltInFunc = fn(Vec<Object>) -> Result<Object, EvalError>;

fn builtin_len(args: Vec<Object>) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::new(
            format!("expected 1 args but got {}.", args.len()).to_string(),
        ));
    }
    match &args[0] {
        Object::STRING(name) => Ok(Object::INTEGER(name.len() as i32)),
        _ => Err(EvalError::new(
            format!(
                "expected string for len(), but got {}",
                args[0].object_type()
            )
            .to_string(),
        )),
    }
}

pub fn get_builtin(name: &String) -> Option<Object> {
    if name.eq("len") {
        return Some(Object::BUILTIN(builtin_len));
    }
    None
}

#[derive(Clone)]
pub enum Object {
    INTEGER(i32),
    BOOLEAN(bool),
    STRING(String),
    RERUTN(Rc<Object>),
    FUNCTION(Vec<Rc<Identifier>>, Rc<BlockStatement>),
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
            Object::BUILTIN(_) => "BUILTIN".to_string(),
        }
    }
}
