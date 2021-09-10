use std::rc::Rc;

use crate::{ast::*, token::Token};

#[derive(Debug)]
pub struct ParseError {
    pub msg: String,
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
const INDEX: i32 = 5;

fn precedence(token: Token) -> i32 {
    match token {
        Token::EQ | Token::NE => EQUALS,
        Token::LT | Token::GT => LESSGREATER,
        Token::PLUS | Token::MINUS => SUM,
        Token::ASTERISK | Token::SLASH => PRODUCT,
        Token::LPAREN => CALL,
        Token::LBRACKET => INDEX,
        _ => LOWEST,
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
        let mut statements: Vec<Rc<dyn Statement>> = Vec::new();
        while self.cur_token() != Token::EOF {
            statements.push(self.parse_statement()?);
        }
        Ok(Program { statements })
    }

    fn parse_statement(&mut self) -> Result<Rc<dyn Statement>, ParseError> {
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
            Token::STRING(_) => panic!("ident should be verified by expect_string()"),
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

    fn expect_string(&mut self) -> Result<Token, ParseError> {
        let ret = match self.cur_token() {
            Token::STRING(value) => Ok(Token::STRING(value)),
            _ => Err(ParseError::new(format!(
                "expected int at {} th token",
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

    fn parse_let_statement(&mut self) -> Result<Rc<dyn Statement>, ParseError> {
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
        Ok(Rc::new(stmt))
    }

    fn parse_block_statement(&mut self) -> Result<Rc<BlockStatement>, ParseError> {
        let mut statements = Vec::new();
        self.expect_token(Token::LBRACE)?;
        while self.cur_token() != Token::EOF && self.cur_token() != Token::RBRACE {
            statements.push(self.parse_statement()?)
        }
        self.expect_token(Token::RBRACE)?;
        Ok(Rc::new(BlockStatement {
            token: Token::LBRACE,
            statements,
        }))
    }

    fn parse_return_statement(&mut self) -> Result<Rc<ReturnStatement>, ParseError> {
        self.expect_token(Token::RETURN)?;
        let return_value = self.parse_expression(LOWEST)?;
        if self.cur_token() == Token::SEMICOLON {
            self.next();
        }
        Ok(Rc::new({
            ReturnStatement {
                token: Token::RETURN,
                return_value,
            }
        }))
    }

    fn parse_expression_statement(&mut self) -> Result<Rc<dyn Statement>, ParseError> {
        let token = self.cur_token();
        let stmt = ExpressionStatement {
            token,
            value: self.parse_expression(LOWEST)?,
        };
        if self.cur_token() == Token::SEMICOLON {
            self.next();
        }
        Ok(Rc::new(stmt))
    }

    fn parse_expression(&mut self, precedence: i32) -> Result<Rc<dyn Expression>, ParseError> {
        let mut left = match self.cur_token() {
            Token::IDENT(_) => self.parse_ident()?,
            Token::INT(_) => self.parse_int_literal()?,
            Token::STRING(_) => self.parse_string_literal()?,
            Token::TRUE | Token::FALSE => self.parse_boolean()?,
            Token::LBRACKET => self.parse_array_literal()?,
            Token::LBRACE => self.parse_dict()?,
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
                Token::LBRACKET => self.parse_index_expression(left)?,
                _ => panic!(),
            };
        }
        Ok(left)
    }

    fn parse_prefix_expression(&mut self) -> Result<Rc<dyn Expression>, ParseError> {
        let token = self.cur_token();
        self.next();
        let right = self.parse_expression(PREFIX)?;
        Ok(Rc::new(PrefixExpression { token, right }))
    }

    fn parse_if_expression(&mut self) -> Result<Rc<dyn Expression>, ParseError> {
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
        Ok(Rc::new(IfExpression {
            token: Token::IF,
            condition,
            consequense,
            alternative,
        }))
    }

    fn parse_infix_expression(
        &mut self,
        left: Rc<dyn Expression>,
    ) -> Result<Rc<dyn Expression>, ParseError> {
        let token = self.cur_token();
        self.next();
        let right = self.parse_expression(precedence(token.clone()))?;
        Ok(Rc::new(InfixExpression { token, left, right }))
    }

    fn parse_function_call(
        &mut self,
        function: Rc<dyn Expression>,
    ) -> Result<Rc<FunctionCall>, ParseError> {
        let args = self.parse_expression_list(Token::LPAREN, Token::RPAREN)?;
        Ok(Rc::new({
            FunctionCall {
                token: Token::LPAREN,
                function,
                args,
            }
        }))
    }

    fn parse_index_expression(
        &mut self,
        left: Rc<dyn Expression>,
    ) -> Result<Rc<IndexExpression>, ParseError> {
        self.expect_token(Token::LBRACKET)?;
        let index = self.parse_expression(LOWEST)?;
        self.expect_token(Token::RBRACKET)?;
        Ok(Rc::new({
            IndexExpression {
                token: Token::LBRACKET,
                left,
                index,
            }
        }))
    }

    fn parse_expression_list(
        &mut self,
        start_token: Token,
        end_token: Token,
    ) -> Result<Vec<Rc<dyn Expression>>, ParseError> {
        self.expect_token(start_token)?;
        let mut params = Vec::new();
        while self.cur_token() != end_token {
            params.push(self.parse_expression(LOWEST)?);
            if self.cur_token() != Token::COMMA {
                break;
            }
            self.expect_token(Token::COMMA)?;
        }
        self.expect_token(end_token)?;
        Ok(params)
    }

    fn parse_function_literal(&mut self) -> Result<Rc<FunctionLiteral>, ParseError> {
        self.expect_token(Token::FUNCTION)?;
        let params = self.parse_function_params()?;
        let body = self.parse_block_statement()?;
        Ok(Rc::new(FunctionLiteral {
            token: Token::FUNCTION,
            params,
            body,
        }))
    }

    fn parse_function_params(&mut self) -> Result<Vec<Rc<Identifier>>, ParseError> {
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

    fn parse_grouped_expression(&mut self) -> Result<Rc<dyn Expression>, ParseError> {
        self.expect_token(Token::LPAREN)?;
        let expression = self.parse_expression(LOWEST)?;
        self.expect_token(Token::RPAREN)?;
        Ok(expression)
    }

    fn parse_int_literal(&mut self) -> Result<Rc<dyn Expression>, ParseError> {
        let token = self.expect_int()?;
        Ok(Rc::new(IntLiteral { token }))
    }

    fn parse_string_literal(&mut self) -> Result<Rc<dyn Expression>, ParseError> {
        let token = self.expect_string()?;
        Ok(Rc::new(StringLiteral { token }))
    }
    fn parse_boolean(&mut self) -> Result<Rc<Boolean>, ParseError> {
        let token = self.expect_bool()?;
        Ok(Rc::new(Boolean { token }))
    }

    fn parse_dict(&mut self) -> Result<Rc<DictLiteral>, ParseError> {
        self.expect_token(Token::LBRACE)?;
        let mut pairs = Vec::new();
        while self.cur_token() != Token::EOF && self.cur_token() != Token::RBRACE {
            let key = self.parse_expression(LOWEST)?;
            self.expect_token(Token::COLON)?;
            let value = self.parse_expression(LOWEST)?;
            pairs.push((key, value));
            if self.cur_token() != Token::COMMA {
                break;
            }
            self.expect_token(Token::COMMA)?;
        }
        self.expect_token(Token::RBRACE)?;
        Ok(Rc::new(DictLiteral {
            token: Token::LBRACE,
            pairs,
        }))
    }

    fn parse_array_literal(&mut self) -> Result<Rc<dyn Expression>, ParseError> {
        Ok(Rc::new(ArrayLiteral {
            token: Token::LBRACKET,
            elements: self.parse_expression_list(Token::LBRACKET, Token::RBRACKET)?,
        }))
    }

    fn parse_ident(&mut self) -> Result<Rc<Identifier>, ParseError> {
        let token = self.expect_ident()?;
        Ok(Rc::new(Identifier { token }))
    }
}

#[cfg(test)]
mod test_parser {
    use crate::parser::Parser;
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
        for (input, expected) in test_cases {
            test_match(input, expected)
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
        for (input, expected) in test_cases {
            test_match(input, expected)
        }
    }

    #[test]
    fn test_fn_literal() {
        let test_cases = [
            ("fn(){}", "fn()"),
            ("fn(x){ x; }", "fn(x)x"),
            ("fn(x, y){ x+y; }", "fn(x, y)(x + y)"),
        ];
        for (input, expected) in test_cases {
            test_match(input, expected)
        }
    }

    #[test]
    fn test_string_literal() {
        let test_cases = [
            ("\"hello\"", "hello"),
            ("\"hello world\"", "hello world"),
            ("\"hello\"+\"world\"", "(hello + world)"),
        ];
        for (input, expected) in test_cases {
            test_match(input, expected)
        }
    }

    #[test]
    fn test_dict_literal() {
        let test_cases = [
            ("{\"hello\": 1, \"world\": 2}", "{hello: 1, world: 2}"),
            ("{\"hello\": 1, 10: 2}", "{hello: 1, 10: 2}"),
            ("{}", "{}"),
        ];
        for (input, expected) in test_cases {
            test_match(input, expected)
        }
    }

    #[test]
    fn test_array_literal() {
        let test_cases = [
            ("[1, 2, 3]", "[1, 2, 3]"),
            ("[1, a, add(1, 2)]", "[1, a, add(1, 2)]"),
        ];
        for (input, expected) in test_cases {
            test_match(input, expected)
        }
        test_is_err("[1, 2}");
        test_is_err("[1, 2");
    }

    #[test]
    fn test_fn_call() {
        let test_cases = [
            ("add(a, b, c)", "add(a, b, c)"),
            ("fn(x, y){ x+y; }(1, 2)", "fn(x, y)(x + y)(1, 2)"),
        ];
        for (input, expected) in test_cases {
            test_match(input, expected)
        }
    }

    fn test_match(input: &str, expected: &str) {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let prog = parser.parse_program().unwrap();
        assert_eq!(prog.statements[0].to_string(), expected);
    }

    fn test_is_err(input: &str) {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        assert!(parser.parse_program().is_err());
    }
}
