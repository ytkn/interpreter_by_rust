use std::collections::HashMap;

use crate::{
    ast::{AstNode, EvalError},
    environment::Environment,
    object::{unwrap_return_value, Object},
};

fn check_arg_num(name: &str, expected: usize, got: usize) -> Result<(), EvalError> {
    if expected != got {
        Err(arg_num_error(name, expected, got))
    } else {
        Ok(())
    }
}

fn arg_num_error(name: &str, expected: usize, got: usize) -> EvalError {
    EvalError::new(format!("{} expected {} args but got {}.", name, expected, got).to_string())
}

fn arg_type_error(name: &str, expected: &str, got: String) -> EvalError {
    EvalError::new(format!("expected {} for {}(), but got {}.", expected, name, got).to_string())
}

fn builtin_len(args: Vec<Object>) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(arg_num_error("len", 1, args.len()));
    }
    match &args[0] {
        Object::STRING(name) => Ok(Object::INTEGER(name.len() as i32)),
        Object::ARRAY(arr) => Ok(Object::INTEGER(arr.len() as i32)),
        Object::DICT(map) => Ok(Object::INTEGER(map.len() as i32)),
        _ => Err(arg_type_error("len", "string/array", args[0].object_type())),
    }
}

fn builtin_first(args: Vec<Object>) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(arg_num_error("first", 1, args.len()));
    }
    match &args[0] {
        Object::ARRAY(arr) => match arr.is_empty() {
            true => Err(EvalError::new("array is empty".to_string())),
            _ => Ok(arr[0].clone()),
        },
        _ => Err(arg_type_error("first", "array", args[0].object_type())),
    }
}

fn builtin_last(args: Vec<Object>) -> Result<Object, EvalError> {
    check_arg_num("last", 1, args.len())?;
    match &args[0] {
        Object::ARRAY(arr) => match arr.is_empty() {
            true => Err(EvalError::new("array is empty".to_string())),
            _ => Ok(arr.last().unwrap().clone()),
        },
        _ => Err(arg_type_error("last", "array", args[0].object_type())),
    }
}

fn builtin_push(args: Vec<Object>) -> Result<Object, EvalError> {
    check_arg_num("push", 2, args.len())?;
    match &args[0] {
        Object::ARRAY(arr) => {
            let mut new_arr = arr.clone();
            new_arr.push(args[1].clone());
            Ok(Object::ARRAY(new_arr))
        }
        _ => Err(arg_type_error("push", "array", args[0].object_type())),
    }
}

fn builtin_tail(args: Vec<Object>) -> Result<Object, EvalError> {
    check_arg_num("tail", 1, args.len())?;
    match &args[0] {
        Object::ARRAY(arr) => match arr.is_empty() {
            true => Err(EvalError::new("array is empty".to_string())),
            _ => Ok(Object::ARRAY(
                arr[1..].into_iter().map(|elm| elm.clone()).collect(),
            )),
        },
        _ => Err(arg_type_error("tail", "array", args[0].object_type())),
    }
}

fn builtin_range(args: Vec<Object>) -> Result<Object, EvalError> {
    check_arg_num("range", 1, args.len())?;
    match &args[0] {
        Object::INTEGER(n) => Ok(Object::ARRAY((0..*n).map(|x| Object::INTEGER(x)).collect())),
        _ => Err(arg_type_error("range", "int", args[0].object_type())),
    }
}

/**
 * NOTE: 環境をキャプチャしないとか色々と不完全
 */
fn builtin_map(args: Vec<Object>) -> Result<Object, EvalError> {
    check_arg_num("map", 2, args.len())?;
    match &args[0] {
        Object::ARRAY(arr) => match &args[1] {
            Object::FUNCTION(params, body) => {
                check_arg_num("second arg in map", 1, params.len())?;
                let name = params[0].token.ident_name().unwrap();
                let mut results = Vec::new();
                let mut func_env = Environment::new();
                for elm in arr {
                    func_env.set(name.clone(), elm.clone());
                    results.push(unwrap_return_value(body.eval(&mut func_env)?));
                }
                Ok(Object::ARRAY(results))
            }
            _ => Err(arg_type_error("map", "callable", args[1].object_type())),
        },
        _ => Err(arg_type_error("map", "array", args[0].object_type())),
    }
}

fn builtin_reduce(args: Vec<Object>) -> Result<Object, EvalError> {
    check_arg_num("reduce", 3, args.len())?;
    match &args[0] {
        Object::ARRAY(arr) => match &args[1] {
            Object::FUNCTION(params, body) => {
                check_arg_num("second arg in reduce", 2, params.len())?;
                let acc_name = params[0].token.ident_name().unwrap();
                let cur_name = params[1].token.ident_name().unwrap();
                let mut func_env = Environment::new();
                let mut acc = (*&args[2]).clone();
                for elm in arr {
                    func_env.set(acc_name.clone(), acc);
                    func_env.set(cur_name.clone(), elm.clone());
                    acc = unwrap_return_value(body.eval(&mut func_env)?);
                }
                Ok(acc)
            }
            _ => Err(arg_type_error("reduce", "callable", args[1].object_type())),
        },
        _ => Err(arg_type_error("reduce", "array", args[0].object_type())),
    }
}

fn builtin_dict_from(args: Vec<Object>) -> Result<Object, EvalError> {
    check_arg_num("dict_from", 2, args.len())?;
    match (&args[0], &args[1]) {
        (Object::ARRAY(keys), Object::ARRAY(values)) => {
            if keys.len() != values.len() {
                return Err(EvalError::new(
                    "size of keys and values should be same".to_string(),
                ));
            }
            let items = keys.into_iter().zip(values);
            let mut map = HashMap::new();
            for (k, v) in items {
                map.insert(k.hash()?, (k.clone(), v.clone()));
            }
            Ok(Object::DICT(map))
        }
        _ => Err(arg_type_error("dict_from", "array", args[0].object_type())),
    }
}

fn builtin_puts(args: Vec<Object>) -> Result<Object, EvalError> {
    for arg in args {
        println!("{}", arg.inspect());
    }
    Ok(Object::NULL)
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
    if name.eq("reduce") {
        return Some(Object::BUILTIN(builtin_reduce));
    }
    if name.eq("range") {
        return Some(Object::BUILTIN(builtin_range));
    }
    if name.eq("tail") {
        return Some(Object::BUILTIN(builtin_tail));
    }
    if name.eq("dict_from") {
        return Some(Object::BUILTIN(builtin_dict_from));
    }
    if name.eq("puts") {
        return Some(Object::BUILTIN(builtin_puts));
    }
    None
}
