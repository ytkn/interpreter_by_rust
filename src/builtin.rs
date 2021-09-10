use crate::{
    ast::{AstNode, EvalError},
    environment::Environment,
    object::{unwrap_return_value, Object},
    token::Token,
};

fn arg_num_error(expected: usize, got: usize) -> EvalError {
    EvalError::new(format!("expected {} args but got {}.", expected, got).to_string())
}

fn builtin_len(args: Vec<Object>) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::new(
            format!("expected 1 args but got {}.", args.len()).to_string(),
        ));
    }
    match &args[0] {
        Object::STRING(name) => Ok(Object::INTEGER(name.len() as i32)),
        Object::ARRAY(arr) => Ok(Object::INTEGER(arr.len() as i32)),
        Object::DICT(map) => Ok(Object::INTEGER(map.len() as i32)),
        _ => Err(EvalError::new(
            format!(
                "expected string or array for len(), but got {}",
                args[0].object_type()
            )
            .to_string(),
        )),
    }
}

fn builtin_first(args: Vec<Object>) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::new(
            format!("expected 1 args but got {}.", args.len()).to_string(),
        ));
    }
    match &args[0] {
        Object::ARRAY(arr) => match arr.is_empty() {
            true => Err(EvalError::new("array is empty".to_string())),
            _ => Ok(arr[0].clone()),
        },
        _ => Err(EvalError::new(
            format!(
                "expected array for first(), but got {}",
                args[0].object_type()
            )
            .to_string(),
        )),
    }
}

fn builtin_last(args: Vec<Object>) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::new(
            format!("expected 1 args but got {}.", args.len()).to_string(),
        ));
    }
    match &args[0] {
        Object::ARRAY(arr) => match arr.is_empty() {
            true => Err(EvalError::new("array is empty".to_string())),
            _ => Ok(arr.last().unwrap().clone()),
        },
        _ => Err(EvalError::new(
            format!(
                "expected array for last(), but got {}",
                args[0].object_type()
            )
            .to_string(),
        )),
    }
}

fn builtin_push(args: Vec<Object>) -> Result<Object, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::new(
            format!("expected 2 args but got {}.", args.len()).to_string(),
        ));
    }
    match &args[0] {
        Object::ARRAY(arr) => {
            let mut new_arr = arr.clone();
            new_arr.push(args[1].clone());
            Ok(Object::ARRAY(new_arr))
        }
        _ => Err(EvalError::new(
            format!(
                "expected array for first(), but got {}",
                args[0].object_type()
            )
            .to_string(),
        )),
    }
}

fn builtin_tail(args: Vec<Object>) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::new(
            format!("expected 1 args but got {}.", args.len()).to_string(),
        ));
    }
    match &args[0] {
        Object::ARRAY(arr) => match arr.is_empty() {
            true => Err(EvalError::new("array is empty".to_string())),
            _ => Ok(Object::ARRAY(
                arr[1..].into_iter().map(|elm| elm.clone()).collect(),
            )),
        },
        _ => Err(EvalError::new(
            format!(
                "expected array for tail(), but got {}",
                args[0].object_type()
            )
            .to_string(),
        )),
    }
}

fn builtin_range(args: Vec<Object>) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::new(
            format!("expected 1 args but got {}.", args.len()).to_string(),
        ));
    }
    match &args[0] {
        Object::INTEGER(n) => Ok(Object::ARRAY((0..*n).map(|x| Object::INTEGER(x)).collect())),
        _ => Err(EvalError::new(
            format!(
                "expected int for range(), but got {}",
                args[0].object_type()
            )
            .to_string(),
        )),
    }
}

/**
 * NOTE: 環境をキャプチャしないとか色々と不完全
 */
fn builtin_map(args: Vec<Object>) -> Result<Object, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::new(
            format!("expected 2 args but got {}.", args.len()).to_string(),
        ));
    }
    match &args[0] {
        Object::ARRAY(arr) => match &args[1] {
            Object::FUNCTION(params, body) => {
                if let Token::IDENT(name) = &params[0].token {
                    let mut results = Vec::new();
                    let mut func_env = Environment::new();
                    for elm in arr {
                        func_env.set(name.clone(), elm.clone());
                        results.push(unwrap_return_value(body.eval(&mut func_env)?));
                    }
                    Ok(Object::ARRAY(results))
                } else {
                    unreachable!()
                }
            }
            _ => Err(EvalError::new(
                "second arg for map should be callable".to_string(),
            )),
        },
        _ => Err(EvalError::new(
            format!(
                "expected array for map(), but got {}",
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
    if name.eq("first") {
        return Some(Object::BUILTIN(builtin_first));
    }
    if name.eq("last") {
        return Some(Object::BUILTIN(builtin_last));
    }
    if name.eq("push") {
        return Some(Object::BUILTIN(builtin_push));
    }
    if name.eq("map") {
        return Some(Object::BUILTIN(builtin_map));
    }
    if name.eq("range") {
        return Some(Object::BUILTIN(builtin_range));
    }
    if name.eq("tail") {
        return Some(Object::BUILTIN(builtin_tail));
    }
    None
}
