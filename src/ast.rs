use crate::token::Token;

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
        _ => LOWEST, // 大丈夫？
    }
}

pub trait AstNode {
    fn token_literal(&self) -> Token;
    fn to_string(&self) -> String;
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
}

impl Expression for Identifier {}

struct IntLiteral {
    token: Token,
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
}

impl Expression for FunctionLiteral {}

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
            "LET({} = {})",
            self.ident.to_string(),
            self.value.to_string()
        )
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
        format!("RETURN({})", self.return_value.to_string())
    }
}

impl Statement for ReturnStatement {}

struct ExpressionStatement {
    token: Token,
    return_value: Box<dyn Expression>,
}

impl AstNode for ExpressionStatement {
    fn token_literal(&self) -> Token {
        self.token.clone()
    }

    fn to_string(&self) -> String {
        format!("{}", self.return_value.to_string())
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
}

impl Expression for InfixExpression {}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Parser {
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

    fn peek_precedence(&self) -> i32 {
        precedence(self.peek_token())
    }

    fn next(&mut self) {
        self.pos += 1;
    }

    pub fn parse_program(&mut self) -> Program {
        let mut statements: Vec<Box<dyn Statement>> = Vec::new();
        while self.cur_token() != Token::EOF {
            statements.push(self.parse_statement());
        }
        Program { statements }
    }

    fn parse_statement(&mut self) -> Box<dyn Statement> {
        match self.cur_token() {
            Token::LET => self.parse_let_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn expect_token(&mut self, token: Token) {
        match token {
            Token::IDENT(_) => panic!("ident should be verified by expect_ident()"),
            Token::INT(_) => panic!("ident should be verified by expect_int()"),
            _ => {
                if self.cur_token() != token {
                    panic!(
                        "token does not match at {} th token ({}). expected {}",
                        self.pos,
                        self.cur_token(),
                        token
                    );
                }
            }
        }
        self.next()
    }

    fn expect_ident(&mut self) -> Token {
        let ret = match self.cur_token() {
            Token::IDENT(name) => Token::IDENT(name),
            _ => panic!("expected ident at {} th token", self.pos),
        };
        self.next();
        ret
    }

    fn expect_int(&mut self) -> Token {
        let ret = match self.cur_token() {
            Token::INT(num) => Token::INT(num),
            _ => panic!("expected int at {} th token", self.pos),
        };
        self.next();
        ret
    }

    fn expect_bool(&mut self) -> Token {
        let token = self.cur_token();
        if token != Token::TRUE && token != Token::FALSE {
            panic!("expected boolean at {} th token", self.pos)
        }
        self.next();
        token
    }

    fn parse_let_statement(&mut self) -> Box<dyn Statement> {
        self.expect_token(Token::LET);
        let ident = Identifier {
            token: self.expect_ident(),
        };
        self.expect_token(Token::ASSIGN);
        let stmt = LetStatement {
            token: Token::LET,
            ident,
            value: self.parse_expression(precedence(Token::ASSIGN)),
        };
        self.expect_token(Token::SEMICOLON);
        Box::new(stmt)
    }

    fn parse_block_statement(&mut self) -> Box<BlockStatement> {
        let mut statements = Vec::new();
        self.expect_token(Token::LBRACE);
        while self.cur_token() != Token::EOF && self.cur_token() != Token::RBRACE {
            statements.push(self.parse_statement())
        }
        self.expect_token(Token::RBRACE);
        Box::new(BlockStatement {
            token: Token::LBRACE,
            statements,
        })
    }

    fn parse_return_statement(&mut self) -> ReturnStatement {
        self.expect_token(Token::RETURN);
        todo!()
    }

    fn parse_expression_statement(&mut self) -> Box<dyn Statement> {
        let token = self.cur_token();
        let stmt = ExpressionStatement {
            token,
            return_value: self.parse_expression(LOWEST),
        };
        if self.cur_token() == Token::SEMICOLON {
            self.next();
        }
        Box::new(stmt)
    }

    fn parse_expression(&mut self, precedence: i32) -> Box<dyn Expression> {
        let mut left = match self.cur_token() {
            Token::IDENT(_) => self.parse_ident(),
            Token::INT(_) => self.parse_int_literal(),
            Token::TRUE | Token::FALSE => self.parse_boolean(),
            Token::BANG | Token::MINUS => self.parse_prefix_expression(),
            Token::LPAREN => self.parse_grouped_expression(),
            Token::IF => self.parse_if_expression(),
            Token::FUNCTION => self.parse_function_literal(),
            _ => panic!("at {} th token {}", self.pos, self.cur_token()),
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
                | Token::GT => self.parse_infix_expression(left),
                _ => panic!(),
            };
        }
        left
    }

    fn parse_prefix_expression(&mut self) -> Box<dyn Expression> {
        let token = self.cur_token();
        self.next();
        let right = self.parse_expression(PREFIX);
        Box::new(PrefixExpression { token, right })
    }

    fn parse_if_expression(&mut self) -> Box<dyn Expression> {
        self.expect_token(Token::IF);
        self.expect_token(Token::LPAREN);
        let condition = self.parse_expression(LOWEST);
        self.expect_token(Token::RPAREN);
        let consequense = self.parse_block_statement();
        let alternative = match self.cur_token() {
            Token::ELSE => {
                self.next();
                Some(self.parse_block_statement())
            }
            _ => None,
        };
        Box::new(IfExpression {
            token: Token::IF,
            condition,
            consequense,
            alternative,
        })
    }

    fn parse_infix_expression(&mut self, left: Box<dyn Expression>) -> Box<dyn Expression> {
        let token = self.cur_token();
        self.next();
        let right = self.parse_expression(precedence(token.clone()));
        Box::new(InfixExpression { token, left, right })
    }

    fn parse_function_literal(&mut self) -> Box<dyn Expression> {
        self.expect_token(Token::FUNCTION);
        let params = self.parse_function_params();
        let body = self.parse_block_statement();
        Box::new(FunctionLiteral{token: Token::FUNCTION, params, body})
    }

    fn parse_function_params(&mut self) -> Vec<Box<Identifier>> {
        self.expect_token(Token::LPAREN);
        let mut params = Vec::new();
        while self.cur_token() != Token::RPAREN {
            params.push(self.parse_ident());
            if self.cur_token() != Token::COMMA {
                break;
            }
            self.expect_token(Token::COMMA);
        }
        self.expect_token(Token::RPAREN);
        params
    }

    fn parse_grouped_expression(&mut self) -> Box<dyn Expression> {
        self.expect_token(Token::LPAREN);
        let expression = self.parse_expression(LOWEST);
        self.expect_token(Token::RPAREN);
        expression
    }

    fn parse_int_literal(&mut self) -> Box<dyn Expression> {
        let token = self.expect_int();
        Box::new(IntLiteral { token })
    }

    fn parse_boolean(&mut self) -> Box<Boolean> {
        let token = self.expect_bool();
        Box::new(Boolean { token })
    }

    fn parse_ident(&mut self) -> Box<Identifier> {
        let token = self.expect_ident();
        Box::new(Identifier { token })
    }
}

#[cfg(test)]
mod test {
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
            let prog = parser.parse_program();
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
            dbg!(&tokens);
            let mut parser = Parser::new(tokens);
            let prog = parser.parse_program();
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
            let prog = parser.parse_program();
            assert_eq!(prog.statements[0].to_string(), exptexced);
        }
    }
}
