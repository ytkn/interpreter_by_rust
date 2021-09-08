use crate::{object::Object, token::Token};

#[derive(Debug)]
pub struct ParseError {
    pub msg: String,
}

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
    fn eval(&self) -> EvalResult;
}

pub trait Statement: AstNode {}

trait Expression: AstNode {}

fn statemens_to_string(statements: &Vec<Box<dyn Statement>>) -> String {
    statements
        .into_iter()
        .map(|stmt| stmt.to_string())
        .collect::<Vec<String>>()
        .join("")
}

fn expressions_to_string(statements: &Vec<Box<dyn Expression>>, sep: &str) -> String {
    statements
        .into_iter()
        .map(|stmt| stmt.to_string())
        .collect::<Vec<String>>()
        .join(sep)
}

struct Identifier {
    token: Token,
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

    fn eval(&self) -> EvalResult {
        todo!()
    }
}

impl Expression for Identifier {}

pub struct IntLiteral {
    token: Token,
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

    fn eval(&self) -> EvalResult {
        Ok(Object::INTEGER(self.value()))
    }
}

impl Expression for IntLiteral {}

struct FunctionLiteral {
    token: Token,
    params: Vec<Box<Identifier>>,
    body: Box<BlockStatement>,
}

impl AstNode for FunctionLiteral {
    fn token_literal(&self) -> Token {
        self.token.clone()
    }

    fn to_string(&self) -> String {
        let params_as_string = |params: &Vec<Box<Identifier>>| -> String {
            params
                .into_iter()
                .map(|p| p.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        }(&self.params);
        format!("fn({}){}", params_as_string, self.body.to_string())
    }

    fn eval(&self) -> EvalResult {
        todo!()
    }
}

impl Expression for FunctionLiteral {}

struct FunctionCall {
    token: Token,
    function: Box<dyn Expression>,
    args: Vec<Box<dyn Expression>>,
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

    fn eval(&self) -> EvalResult {
        todo!()
    }
}

impl Expression for FunctionCall {}

struct Boolean {
    token: Token,
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

    fn eval(&self) -> EvalResult {
        match self.token {
            Token::TRUE => Ok(Object::BOOLEAN(true)),
            Token::FALSE => Ok(Object::BOOLEAN(false)),
            _ => panic!(),
        }
    }
}

impl Expression for Boolean {}

struct LetStatement {
    token: Token,
    ident: Identifier,
    value: Box<dyn Expression>,
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

    fn eval(&self) -> EvalResult {
        todo!()
    }
}

impl Statement for LetStatement {}

struct ReturnStatement {
    token: Token,
    return_value: Box<dyn Expression>,
}

impl AstNode for ReturnStatement {
    fn token_literal(&self) -> Token {
        self.token.clone()
    }

    fn to_string(&self) -> String {
        format!("return {}", self.return_value.to_string())
    }

    fn eval(&self) -> EvalResult {
        let ret = self.return_value.eval()?;
        Ok(Object::RERUTN(Box::new(ret)))
    }
}

impl Statement for ReturnStatement {}

struct ExpressionStatement {
    token: Token,
    value: Box<dyn Expression>,
}

impl AstNode for ExpressionStatement {
    fn token_literal(&self) -> Token {
        self.token.clone()
    }

    fn to_string(&self) -> String {
        format!("{}", self.value.to_string())
    }

    fn eval(&self) -> EvalResult {
        self.value.eval()
    }
}

impl Statement for ExpressionStatement {}

struct IfExpression {
    token: Token,
    condition: Box<dyn Expression>,
    consequense: Box<BlockStatement>,
    alternative: Option<Box<BlockStatement>>,
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

    fn eval(&self) -> EvalResult {
        let cond = self.condition.eval()?;
        match cond {
            Object::BOOLEAN(false) | Object::NULL => match &self.alternative {
                Some(block) => block.eval(),
                _ => Ok(Object::NULL),
            },
            _ => self.consequense.eval(),
        }
    }
}

impl Expression for IfExpression {}

struct BlockStatement {
    token: Token,
    statements: Vec<Box<dyn Statement>>,
}

impl AstNode for BlockStatement {
    fn token_literal(&self) -> Token {
        self.token.clone()
    }

    fn to_string(&self) -> String {
        statemens_to_string(&self.statements)
    }

    fn eval(&self) -> EvalResult {
        eval_statements(&self.statements)
    }
}

impl Statement for BlockStatement {}

struct PrefixExpression {
    token: Token,
    right: Box<dyn Expression>,
}

impl AstNode for PrefixExpression {
    fn token_literal(&self) -> Token {
        self.token.clone()
    }
    fn to_string(&self) -> String {
        format!("({}{})", self.token, self.right.to_string())
    }

    fn eval(&self) -> EvalResult {
        let right = self.right.eval()?;
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

struct InfixExpression {
    token: Token,
    left: Box<dyn Expression>,
    right: Box<dyn Expression>,
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

    fn eval(&self) -> EvalResult {
        let left_val = match self.left.eval()? {
            Object::INTEGER(x) => x,
            _ => Err(EvalError::new(format!("expected int for '{}'", self.token)))?,
        };
        let right_val = match self.right.eval()? {
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
    pub statements: Vec<Box<dyn Statement>>,
}

fn eval_statements(statements: &Vec<Box<dyn Statement>>) -> EvalResult {
    let mut ret = Object::NULL;
    for stmt in statements {
        ret = stmt.eval()?;
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

    fn eval(&self) -> EvalResult {
        let mut ret = Object::NULL;
        for stmt in &self.statements {
            ret = stmt.eval()?;
            if let Object::RERUTN(val) = ret {
                return Ok(*val);
            }
        }
        Ok(ret)
    }
}

#[cfg(test)]
mod test_evaluator {
    use crate::ast::Parser;
    use crate::token::Lexer;

    use super::{AstNode, EvalResult};
    #[test]
    fn test_eval_int_literal() {
        assert_eq!("5", test_eval("5").unwrap().inspect());
        assert_eq!("10", test_eval("10").unwrap().inspect());
    }
    #[test]
    fn test_eval_bool_literal() {
        assert_eq!("true", test_eval("true").unwrap().inspect());
        assert_eq!("false", test_eval("false").unwrap().inspect());
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
    }

    #[test]
    fn test_eval_prefix_operator() {
        assert_eq!("false", test_eval("!true").unwrap().inspect());
        assert_eq!("true", test_eval("!false").unwrap().inspect());
        assert_eq!("false", test_eval("!5").unwrap().inspect());
        assert_eq!("true", test_eval("!!5").unwrap().inspect());
        assert_eq!("false", test_eval("!!false").unwrap().inspect());
        assert_eq!("-5", test_eval("-5").unwrap().inspect());
        assert_eq!("-10", test_eval("-10").unwrap().inspect());
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
    fn test_return() {
        test_eval_match("if(true){ return 10 }", "10");
        test_eval_match("if(true){ if(true) { return 10 } return 5 }", "10");
    }

    fn test_eval_match(input: &str, expected: &str) {
        assert_eq!(test_eval(input).unwrap().inspect(), expected);
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
        prog.eval()
    }
}

impl ParseError {
    fn new(msg: String) -> ParseError {
        ParseError { msg }
    }
}

const LOWEST: i32 = 0;
const EQUALS: i32 = 1;
const LESSGREATER: i32 = 2;
const SUM: i32 = 3;
const PRODUCT: i32 = 4;
const PREFIX: i32 = 5;
const CALL: i32 = 6;

fn precedence(token: Token) -> i32 {
    match token {
        Token::EQ | Token::NE => EQUALS,
        Token::LT | Token::GT => LESSGREATER,
        Token::PLUS | Token::MINUS => SUM,
        Token::ASTERISK | Token::SLASH => PRODUCT,
        Token::LPAREN => CALL,
        _ => LOWEST, // 大丈夫？
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, pos: 0 }
    }
    fn cur_token(&self) -> Token {
        if self.pos < self.tokens.len() {
            self.tokens[self.pos].clone()
        } else {
            Token::EOF
        }
    }

    fn peek_token(&self) -> Token {
        if self.pos + 1 < self.tokens.len() {
            self.tokens[self.pos + 1].clone()
        } else {
            Token::EOF
        }
    }

    fn cur_precedence(&self) -> i32 {
        precedence(self.cur_token())
    }

    fn next(&mut self) {
        self.pos += 1;
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut statements: Vec<Box<dyn Statement>> = Vec::new();
        while self.cur_token() != Token::EOF {
            statements.push(self.parse_statement()?);
        }
        Ok(Program { statements })
    }

    fn parse_statement(&mut self) -> Result<Box<dyn Statement>, ParseError> {
        let stmt = match self.cur_token() {
            Token::LET => self.parse_let_statement()?,
            Token::RETURN => self.parse_return_statement()?,
            _ => self.parse_expression_statement()?,
        };
        Ok(stmt)
    }

    fn expect_token(&mut self, token: Token) -> Result<Token, ParseError> {
        let ret = match token {
            Token::IDENT(_) => panic!("ident should be verified by expect_ident()"),
            Token::INT(_) => panic!("ident should be verified by expect_int()"),
            _ => {
                if self.cur_token() != token {
                    Err(ParseError::new(format!(
                        "token does not match at {} th token '{}'. expected {}",
                        self.pos,
                        self.cur_token(),
                        token
                    )))
                } else {
                    Ok(self.cur_token())
                }
            }
        };
        self.next();
        ret
    }

    fn expect_ident(&mut self) -> Result<Token, ParseError> {
        let ret = match self.cur_token() {
            Token::IDENT(name) => Ok(Token::IDENT(name)),
            _ => Err(ParseError::new(format!(
                "expected ident at {} th token",
                self.pos
            ))),
        };
        self.next();
        ret
    }

    fn expect_int(&mut self) -> Result<Token, ParseError> {
        let ret = match self.cur_token() {
            Token::INT(num) => Ok(Token::INT(num)),
            _ => Err(ParseError::new(format!(
                "expected int at {} th token",
                self.pos
            ))),
        };
        self.next();
        ret
    }

    fn expect_bool(&mut self) -> Result<Token, ParseError> {
        let token = self.cur_token();
        if token != Token::TRUE && token != Token::FALSE {
            return Err(ParseError::new(format!(
                "expected boolean at {} th token",
                self.pos
            )));
        }
        self.next();
        Ok(token)
    }

    fn parse_let_statement(&mut self) -> Result<Box<dyn Statement>, ParseError> {
        self.expect_token(Token::LET)?;
        let ident = Identifier {
            token: self.expect_ident()?,
        };
        self.expect_token(Token::ASSIGN)?;
        let stmt = LetStatement {
            token: Token::LET,
            ident,
            value: self.parse_expression(precedence(Token::ASSIGN))?,
        };
        if self.cur_token() == Token::SEMICOLON {
            self.next();
        }
        Ok(Box::new(stmt))
    }

    fn parse_block_statement(&mut self) -> Result<Box<BlockStatement>, ParseError> {
        let mut statements = Vec::new();
        self.expect_token(Token::LBRACE)?;
        while self.cur_token() != Token::EOF && self.cur_token() != Token::RBRACE {
            statements.push(self.parse_statement()?)
        }
        self.expect_token(Token::RBRACE)?;
        Ok(Box::new(BlockStatement {
            token: Token::LBRACE,
            statements,
        }))
    }

    fn parse_return_statement(&mut self) -> Result<Box<ReturnStatement>, ParseError> {
        self.expect_token(Token::RETURN)?;
        let return_value = self.parse_expression(LOWEST)?;
        if self.cur_token() == Token::SEMICOLON {
            self.next();
        }
        Ok(Box::new({
            ReturnStatement {
                token: Token::RETURN,
                return_value,
            }
        }))
    }

    fn parse_expression_statement(&mut self) -> Result<Box<dyn Statement>, ParseError> {
        let token = self.cur_token();
        let stmt = ExpressionStatement {
            token,
            value: self.parse_expression(LOWEST)?,
        };
        if self.cur_token() == Token::SEMICOLON {
            self.next();
        }
        Ok(Box::new(stmt))
    }

    fn parse_expression(&mut self, precedence: i32) -> Result<Box<dyn Expression>, ParseError> {
        let mut left = match self.cur_token() {
            Token::IDENT(_) => self.parse_ident()?,
            Token::INT(_) => self.parse_int_literal()?,
            Token::TRUE | Token::FALSE => self.parse_boolean()?,
            Token::BANG | Token::MINUS => self.parse_prefix_expression()?,
            Token::LPAREN => self.parse_grouped_expression()?,
            Token::IF => self.parse_if_expression()?,
            Token::FUNCTION => self.parse_function_literal()?,
            _ => Err(ParseError::new(format!(
                "at {} th token {}",
                self.pos,
                self.cur_token()
            )))?,
        };
        while self.peek_token() != Token::SEMICOLON && precedence < self.cur_precedence() {
            left = match self.cur_token() {
                Token::PLUS
                | Token::MINUS
                | Token::ASTERISK
                | Token::SLASH
                | Token::EQ
                | Token::NE
                | Token::LT
                | Token::GT => self.parse_infix_expression(left)?,
                Token::LPAREN => self.parse_function_call(left)?,
                _ => panic!(),
            };
        }
        Ok(left)
    }

    fn parse_prefix_expression(&mut self) -> Result<Box<dyn Expression>, ParseError> {
        let token = self.cur_token();
        self.next();
        let right = self.parse_expression(PREFIX)?;
        Ok(Box::new(PrefixExpression { token, right }))
    }

    fn parse_if_expression(&mut self) -> Result<Box<dyn Expression>, ParseError> {
        self.expect_token(Token::IF)?;
        self.expect_token(Token::LPAREN)?;
        let condition = self.parse_expression(LOWEST)?;
        self.expect_token(Token::RPAREN)?;
        let consequense = self.parse_block_statement()?;
        let alternative = match self.cur_token() {
            Token::ELSE => {
                self.next();
                Some(self.parse_block_statement()?)
            }
            _ => None,
        };
        Ok(Box::new(IfExpression {
            token: Token::IF,
            condition,
            consequense,
            alternative,
        }))
    }

    fn parse_infix_expression(
        &mut self,
        left: Box<dyn Expression>,
    ) -> Result<Box<dyn Expression>, ParseError> {
        let token = self.cur_token();
        self.next();
        let right = self.parse_expression(precedence(token.clone()))?;
        Ok(Box::new(InfixExpression { token, left, right }))
    }

    fn parse_function_call(
        &mut self,
        function: Box<dyn Expression>,
    ) -> Result<Box<FunctionCall>, ParseError> {
        let args = self.parse_call_args()?;
        Ok(Box::new({
            FunctionCall {
                token: Token::LPAREN,
                function,
                args,
            }
        }))
    }

    fn parse_call_args(&mut self) -> Result<Vec<Box<dyn Expression>>, ParseError> {
        self.expect_token(Token::LPAREN)?;
        let mut params = Vec::new();
        while self.cur_token() != Token::RPAREN {
            params.push(self.parse_expression(LOWEST)?);
            if self.cur_token() != Token::COMMA {
                break;
            }
            self.expect_token(Token::COMMA)?;
        }
        self.expect_token(Token::RPAREN)?;
        Ok(params)
    }

    fn parse_function_literal(&mut self) -> Result<Box<FunctionLiteral>, ParseError> {
        self.expect_token(Token::FUNCTION)?;
        let params = self.parse_function_params()?;
        let body = self.parse_block_statement()?;
        Ok(Box::new(FunctionLiteral {
            token: Token::FUNCTION,
            params,
            body,
        }))
    }

    fn parse_function_params(&mut self) -> Result<Vec<Box<Identifier>>, ParseError> {
        self.expect_token(Token::LPAREN)?;
        let mut params = Vec::new();
        while self.cur_token() != Token::RPAREN {
            params.push(self.parse_ident()?);
            if self.cur_token() != Token::COMMA {
                break;
            }
            self.expect_token(Token::COMMA)?;
        }
        self.expect_token(Token::RPAREN)?;
        Ok(params)
    }

    fn parse_grouped_expression(&mut self) -> Result<Box<dyn Expression>, ParseError> {
        self.expect_token(Token::LPAREN)?;
        let expression = self.parse_expression(LOWEST)?;
        self.expect_token(Token::RPAREN)?;
        Ok(expression)
    }

    fn parse_int_literal(&mut self) -> Result<Box<dyn Expression>, ParseError> {
        let token = self.expect_int()?;
        Ok(Box::new(IntLiteral { token }))
    }

    fn parse_boolean(&mut self) -> Result<Box<Boolean>, ParseError> {
        let token = self.expect_bool()?;
        Ok(Box::new(Boolean { token }))
    }

    fn parse_ident(&mut self) -> Result<Box<Identifier>, ParseError> {
        let token = self.expect_ident()?;
        Ok(Box::new(Identifier { token }))
    }
}

#[cfg(test)]
mod test_parser {
    use crate::ast::Parser;
    use crate::token::Lexer;

    #[test]
    fn test_operator_precedence() {
        let test_cases = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a+b+ c", "((a + b) + c)"),
            ("a+b - c", "((a + b) - c)"),
            ("a*b*c", "((a * b) * c)"),
            ("a*b/c", "((a * b) / c)"),
            ("a+b/c", "(a + (b / c))"),
            ("a+b*c+d/e-f", "(((a + (b * c)) + (d / e)) - f)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 > 4 != 3 < 4", "((5 > 4) != (3 < 4))"),
            ("5 > 4 == true", "((5 > 4) == true)"),
            ("5 < 4 != true", "((5 < 4) != true)"),
            ("5 < 4 == false", "((5 < 4) == false)"),
            ("(a+b)*c", "((a + b) * c)"),
            ("-(5+5)", "(-(5 + 5))"),
        ];
        for (input, exptexced) in test_cases {
            let mut lexer = Lexer::new(input);
            let mut tokens = Vec::new();
            while !lexer.at_eof() {
                let tok = lexer.next_token();
                tokens.push(tok);
            }
            let mut parser = Parser::new(tokens);
            let prog = parser.parse_program().unwrap();
            assert_eq!(prog.statements.len(), 1);
            assert_eq!(prog.statements[0].to_string(), exptexced);
        }
    }
    #[test]
    fn test_if_expression() {
        let test_cases = [(
            "if(x < y) {
                y-x
            } else {
                x-y
            }",
            "if(x < y) (y - x) else (x - y)",
        )];
        for (input, exptexced) in test_cases {
            let mut lexer = Lexer::new(input);
            let mut tokens = Vec::new();
            while !lexer.at_eof() {
                let tok = lexer.next_token();
                tokens.push(tok);
            }
            let mut parser = Parser::new(tokens);
            let prog = parser.parse_program().unwrap();
            assert_eq!(prog.statements[0].to_string(), exptexced);
        }
    }

    #[test]
    fn test_fn_literal() {
        let test_cases = [
            ("fn(){}", "fn()"),
            ("fn(x){ x; }", "fn(x)x"),
            ("fn(x, y){ x+y; }", "fn(x, y)(x + y)"),
        ];
        for (input, exptexced) in test_cases {
            let mut lexer = Lexer::new(input);
            let mut tokens = Vec::new();
            while !lexer.at_eof() {
                let tok = lexer.next_token();
                tokens.push(tok);
            }
            let mut parser = Parser::new(tokens);
            let prog = parser.parse_program().unwrap();
            assert_eq!(prog.statements[0].to_string(), exptexced);
        }
    }

    #[test]
    fn test_fn_call() {
        let test_cases = [
            ("add(a, b, c)", "add(a, b, c)"),
            ("fn(x, y){ x+y; }(1, 2)", "fn(x, y)(x + y)(1, 2)"),
        ];
        for (input, exptexced) in test_cases {
            let mut lexer = Lexer::new(input);
            let mut tokens = Vec::new();
            while !lexer.at_eof() {
                let tok = lexer.next_token();
                tokens.push(tok);
            }
            let mut parser = Parser::new(tokens);
            let prog = parser.parse_program().unwrap();
            assert_eq!(prog.statements[0].to_string(), exptexced);
        }
    }
}
