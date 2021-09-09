use std::rc::Rc;

use crate::{environment::Environment, object::Object, token::Token};

#[derive(Debug)]
pub struct EvalError {
    pub msg: String,
}

impl EvalError {
    fn new(msg: String) -> EvalError {
        EvalError { msg }
    }
}

type EvalResult = Result<Object, EvalError>;

pub trait AstNode {
    fn token_literal(&self) -> Token;
    fn to_string(&self) -> String;
    fn eval(&self, env: &mut Environment) -> EvalResult;
}

pub trait Statement: AstNode {}

pub trait Expression: AstNode {}

fn statemens_to_string(statements: &Vec<Rc<dyn Statement>>) -> String {
    statements
        .into_iter()
        .map(|stmt| stmt.to_string())
        .collect::<Vec<String>>()
        .join("")
}

fn expressions_to_string(statements: &Vec<Rc<dyn Expression>>, sep: &str) -> String {
    statements
        .into_iter()
        .map(|stmt| stmt.to_string())
        .collect::<Vec<String>>()
        .join(sep)
}

pub struct Identifier {
    pub token: Token,
}

impl AstNode for Identifier {
    fn token_literal(&self) -> Token {
        self.token.clone()
    }

    fn to_string(&self) -> String {
        match &self.token {
            Token::IDENT(name) => name.clone(),
            _ => panic!(),
        }
    }

    fn eval(&self, env: &mut Environment) -> EvalResult {
        let name = match &self.token {
            Token::IDENT(name) => name,
            _ => unreachable!(),
        };
        match env.get(name) {
            Some(x) => Ok(x),
            None => Err(EvalError::new(format!("{} not found", name))),
        }
    }
}

impl Expression for Identifier {}

pub struct IntLiteral {
    pub token: Token,
}

impl IntLiteral {
    fn value(&self) -> i32 {
        match self.token {
            Token::INT(x) => x,
            _ => panic!(),
        }
    }
}

impl AstNode for IntLiteral {
    fn token_literal(&self) -> Token {
        self.token.clone()
    }

    fn to_string(&self) -> String {
        match self.token {
            Token::INT(x) => x.to_string(),
            _ => panic!(),
        }
    }

    fn eval(&self, _env: &mut Environment) -> EvalResult {
        Ok(Object::INTEGER(self.value()))
    }
}

impl Expression for IntLiteral {}

pub struct FunctionLiteral {
    pub token: Token,
    pub params: Vec<Rc<Identifier>>,
    pub body: Rc<BlockStatement>,
}

impl AstNode for FunctionLiteral {
    fn token_literal(&self) -> Token {
        self.token.clone()
    }

    fn to_string(&self) -> String {
        let params_as_string = |params: &Vec<Rc<Identifier>>| -> String {
            params
                .into_iter()
                .map(|p| p.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        }(&self.params);
        format!("fn({}){}", params_as_string, self.body.to_string())
    }

    fn eval(&self, _env: &mut Environment) -> EvalResult {
        Ok(Object::FUNCTION(self.params.clone(), self.body.clone()))
    }
}

impl Expression for FunctionLiteral {}

pub struct FunctionCall {
    pub token: Token,
    pub function: Rc<dyn Expression>,
    pub args: Vec<Rc<dyn Expression>>,
}

fn eval_expressions(
    expressions: &Vec<Rc<dyn Expression>>,
    env: &mut Environment,
) -> Result<Vec<Object>, EvalError> {
    let mut result = Vec::new();
    for exp in expressions {
        result.push(exp.eval(env)?)
    }
    Ok(result)
}

fn unwrap_return_value(obj: Object) -> Object {
    match obj {
        Object::RERUTN(x) => (*x).clone(),
        x => x,
    }
}

impl FunctionCall {
    fn arg_error(expected: usize, got: usize) -> EvalError {
        EvalError::new(format!("expected {} args but got {} args", expected, got))
    }
}

impl AstNode for FunctionCall {
    fn token_literal(&self) -> Token {
        self.token.clone()
    }

    fn to_string(&self) -> String {
        format!(
            "{}({})",
            self.function.to_string(),
            expressions_to_string(&self.args, ", ")
        )
    }

    fn eval(&self, env: &mut Environment) -> EvalResult {
        let func = self.function.eval(env)?;
        let args = eval_expressions(&self.args, env)?;
        match func {
            Object::FUNCTION(params, body) => {
                if args.len() != params.len() {
                    Err(FunctionCall::arg_error(params.len(), args.len()))
                } else {
                    let mut func_env = Environment::new_with_outer(env);
                    params.into_iter().zip(args.into_iter()).for_each(
                        |(ident, value)| match &ident.token {
                            Token::IDENT(name) => func_env.set(name.clone(), value),
                            _ => unreachable!(),
                        },
                    );
                    Ok(unwrap_return_value(body.eval(&mut func_env)?))
                }
            }
            _ => Err(EvalError::new("not callable".to_string())),
        }
    }
}

impl Expression for FunctionCall {}

pub struct Boolean {
    pub token: Token,
}

impl AstNode for Boolean {
    fn token_literal(&self) -> Token {
        self.token.clone()
    }

    fn to_string(&self) -> String {
        match self.token {
            Token::TRUE => "true".to_string(),
            Token::FALSE => "false".to_string(),
            _ => panic!(),
        }
    }

    fn eval(&self, _env: &mut Environment) -> EvalResult {
        match self.token {
            Token::TRUE => Ok(Object::BOOLEAN(true)),
            Token::FALSE => Ok(Object::BOOLEAN(false)),
            _ => panic!(),
        }
    }
}

impl Expression for Boolean {}

pub struct LetStatement {
    pub token: Token,
    pub ident: Identifier,
    pub value: Rc<dyn Expression>,
}

impl AstNode for LetStatement {
    fn token_literal(&self) -> Token {
        self.token.clone()
    }

    fn to_string(&self) -> String {
        format!(
            "let {} = {}",
            self.ident.to_string(),
            self.value.to_string()
        )
    }

    fn eval(&self, env: &mut Environment) -> EvalResult {
        let value = self.value.eval(env)?;
        match &self.ident.token {
            Token::IDENT(name) => env.set(name.clone(), value.clone()),
            _ => unreachable!(),
        }
        Ok(value)
    }
}

impl Statement for LetStatement {}

pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Rc<dyn Expression>,
}

impl AstNode for ReturnStatement {
    fn token_literal(&self) -> Token {
        self.token.clone()
    }

    fn to_string(&self) -> String {
        format!("return {}", self.return_value.to_string())
    }

    fn eval(&self, env: &mut Environment) -> EvalResult {
        let ret = self.return_value.eval(env)?;
        Ok(Object::RERUTN(Rc::new(ret)))
    }
}

impl Statement for ReturnStatement {}

pub struct ExpressionStatement {
    pub token: Token,
    pub value: Rc<dyn Expression>,
}

impl AstNode for ExpressionStatement {
    fn token_literal(&self) -> Token {
        self.token.clone()
    }

    fn to_string(&self) -> String {
        format!("{}", self.value.to_string())
    }

    fn eval(&self, env: &mut Environment) -> EvalResult {
        self.value.eval(env)
    }
}

impl Statement for ExpressionStatement {}

pub struct IfExpression {
    pub token: Token,
    pub condition: Rc<dyn Expression>,
    pub consequense: Rc<BlockStatement>,
    pub alternative: Option<Rc<BlockStatement>>,
}

impl AstNode for IfExpression {
    fn token_literal(&self) -> Token {
        self.token.clone()
    }

    fn to_string(&self) -> String {
        match &self.alternative {
            Some(statements) => format!(
                "if{} {} else {}",
                self.condition.to_string(),
                self.consequense.to_string(),
                statements.to_string()
            ),
            None => format!(
                "if{} {}",
                self.condition.to_string(),
                self.consequense.to_string(),
            ),
        }
    }

    fn eval(&self, env: &mut Environment) -> EvalResult {
        let cond = self.condition.eval(env)?;
        match cond {
            Object::BOOLEAN(false) | Object::NULL => match &self.alternative {
                Some(block) => block.eval(env),
                _ => Ok(Object::NULL),
            },
            _ => self.consequense.eval(env),
        }
    }
}

impl Expression for IfExpression {}

pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Rc<dyn Statement>>,
}

impl AstNode for BlockStatement {
    fn token_literal(&self) -> Token {
        self.token.clone()
    }

    fn to_string(&self) -> String {
        statemens_to_string(&self.statements)
    }

    fn eval(&self, env: &mut Environment) -> EvalResult {
        eval_statements(&self.statements, env)
    }
}

impl Statement for BlockStatement {}

pub struct PrefixExpression {
    pub token: Token,
    pub right: Rc<dyn Expression>,
}

impl AstNode for PrefixExpression {
    fn token_literal(&self) -> Token {
        self.token.clone()
    }
    fn to_string(&self) -> String {
        format!("({}{})", self.token, self.right.to_string())
    }

    fn eval(&self, env: &mut Environment) -> EvalResult {
        let right = self.right.eval(env)?;
        match self.token {
            Token::BANG => match right {
                Object::BOOLEAN(f) => Ok(Object::BOOLEAN(!f)),
                Object::NULL => Ok(Object::BOOLEAN(true)),
                _ => Ok(Object::BOOLEAN(false)),
            },
            Token::MINUS => match right {
                Object::INTEGER(x) => Ok(Object::INTEGER(-x)),
                _ => Err(EvalError::new("expected int before '-'".to_string())),
            },
            _ => unreachable!(),
        }
    }
}

impl Expression for PrefixExpression {}

pub struct InfixExpression {
    pub token: Token,
    pub left: Rc<dyn Expression>,
    pub right: Rc<dyn Expression>,
}

impl AstNode for InfixExpression {
    fn token_literal(&self) -> Token {
        self.token.clone()
    }
    fn to_string(&self) -> String {
        format!(
            "({} {} {})",
            self.left.to_string(),
            self.token,
            self.right.to_string()
        )
    }

    fn eval(&self, env: &mut Environment) -> EvalResult {
        let left_val = match self.left.eval(env)? {
            Object::INTEGER(x) => x,
            _ => Err(EvalError::new(format!("expected int for '{}'", self.token)))?,
        };
        let right_val = match self.right.eval(env)? {
            Object::INTEGER(x) => x,
            _ => Err(EvalError::new(format!("expected int for '{}'", self.token)))?,
        };

        match self.token {
            Token::ASTERISK => Ok(Object::INTEGER(left_val * right_val)),
            Token::SLASH => Ok(Object::INTEGER(left_val / right_val)),
            Token::PLUS => Ok(Object::INTEGER(left_val + right_val)),
            Token::MINUS => Ok(Object::INTEGER(left_val - right_val)),
            Token::LT => Ok(Object::BOOLEAN(left_val < right_val)),
            Token::GT => Ok(Object::BOOLEAN(left_val > right_val)),
            Token::EQ => Ok(Object::BOOLEAN(left_val == right_val)),
            Token::NE => Ok(Object::BOOLEAN(left_val != right_val)),
            _ => unreachable!(),
        }
    }
}

impl Expression for InfixExpression {}

pub struct Program {
    pub statements: Vec<Rc<dyn Statement>>,
}

fn eval_statements(statements: &Vec<Rc<dyn Statement>>, env: &mut Environment) -> EvalResult {
    let mut ret = Object::NULL;
    for stmt in statements {
        ret = stmt.eval(env)?;
        if let Object::RERUTN(_) = ret {
            break;
        }
    }
    Ok(ret)
}

impl AstNode for Program {
    fn token_literal(&self) -> Token {
        todo!()
    }

    fn to_string(&self) -> String {
        todo!()
    }

    fn eval(&self, env: &mut Environment) -> EvalResult {
        let mut ret = Object::NULL;
        for stmt in &self.statements {
            ret = stmt.eval(env)?;
            if let Object::RERUTN(val) = ret {
                return Ok(Object::RERUTN(val));
            }
        }
        Ok(ret)
    }
}

#[cfg(test)]
mod test_evaluator {
    use crate::environment::Environment;
    use crate::parser::Parser;
    use crate::token::Lexer;

    use super::{AstNode, EvalResult};
    #[test]
    fn test_eval_int_literal() {
        test_eval_match("5", "5");
        test_eval_match("10", "10");
    }
    #[test]
    fn test_eval_bool_literal() {
        test_eval_match("true", "true");
        test_eval_match("false", "false");
    }

    #[test]
    fn test_eval_infix_operator() {
        test_eval_match("5+5", "10");
        test_eval_match("5+5", "10");
        test_eval_match("10*10+9", "109");
        test_eval_match("10+10*9", "100");
        test_eval_match("10 > 10*9", "false");
        test_eval_match("10 < 10*9", "true");
        test_eval_match("100 == 10+10*9", "true");
        test_eval_match("100 != 10+10*9", "false");
        test_eval_match("180 != (10+10)*9", "false");
        test_is_err("5+true");
        test_is_err("true+false");
    }

    #[test]
    fn test_eval_prefix_operator() {
        test_eval_match("!true", "false");
        test_eval_match("!false", "true");
        test_eval_match("!5", "false");
        test_eval_match("!!5", "true");
        test_eval_match("!!false", "false");
        test_eval_match("-5", "-5");
        test_eval_match("-10", "-10");
    }

    #[test]
    fn test_if_else() {
        test_eval_match("if(true){ 10 }", "10");
        test_eval_match("if(true){ 10 } else { 20 }", "10");
        test_eval_match("if(false){ 10 } else { 20 }", "20");
        test_eval_match("if(1*10 == 20){ 10 } else { 20 }", "20");
        test_eval_match("if(1+10/10 == 2){ 10 } else { 20 }", "10");
    }

    #[test]
    fn test_func() {
        test_eval_match("let add = fn(a, b){ return a+b; }; add(1, 5)", "6");
        test_eval_match("let add = fn(a, b){ a+b; }; add(1, 5)", "6");
        test_eval_match(
            "let fac = fn(n){ if(n == 0){ return 1 } else { return fac(n-1)*n}}; fac(10)",
            "3628800",
        );
        test_eval_match(
            "let fib = fn(n){ if(n < 3){ return 1 } else { return fib(n-1)+fib(n-2)}}; fib(15)",
            "610",
        );
        test_is_err("let add = fn(a, b){ return a+b; }; add(1, 5, 9)");
        test_is_err("let add = fn(a, b){ return a+b; }; add(1)");
    }

    #[test]
    fn test_return() {
        test_eval_match("if(true){ return 10 }", "10");
        test_eval_match("if(true){ if(true) { return 10 } return 5 }", "10");
    }

    #[test]
    fn test_let() {
        test_eval_match("let x = 10; x", "10");
    }

    fn test_eval_match(input: &str, expected: &str) {
        assert_eq!(test_eval(input).unwrap().inspect(), expected);
    }

    fn test_is_err(input: &str) {
        assert!(test_eval(input).is_err());
    }

    fn test_eval(input: &str) -> EvalResult {
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        while !lexer.at_eof() {
            let tok = lexer.next_token();
            tokens.push(tok);
        }
        let mut parser = Parser::new(tokens);
        let prog = parser.parse_program().unwrap();
        let mut env = Environment::new();
        prog.eval(&mut env)
    }
}
