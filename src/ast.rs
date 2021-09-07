use crate::token::Token;

const LOWEST: i32 = 0;
const EQUALS: i32 = 1;
const LESSGREATER: i32 = 2;
const SUM: i32 = 3;
const PRODUCT: i32 = 4;

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
        format!("({}, {})", self.token, self.return_value.to_string())
    }
}

impl Statement for ExpressionStatement {}

struct PrefixExpression {
    token: Token,
    right: Box<dyn Expression>,
}

impl AstNode for PrefixExpression {
    fn token_literal(&self) -> Token {
        self.token.clone()
    }
    fn to_string(&self) -> String {
        format!("({}, {})", self.token, self.right.to_string())
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
            match self.cur_token() {
                Token::LET => {
                    statements.push(Box::new(self.parse_let_statement()));
                }
                _ => unreachable!(),
            }
        }
        Program { statements }
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

    fn parse_let_statement(&mut self) -> LetStatement {
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
        stmt
    }

    fn parse_expression(&mut self, precedence: i32) -> Box<dyn Expression> {
        let mut left = match self.cur_token() {
            Token::IDENT(_) => self.parse_ident(),
            Token::INT(_) => self.parse_int_literal(),
            Token::BANG | Token::MINUS => self.parse_prefix_expression(),
            _ => panic!("{}", self.cur_token()),
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

    fn parse_return_statement(&mut self) -> ReturnStatement {
        self.expect_token(Token::RETURN);
        todo!()
    }

    fn parse_expression_statement(&mut self) -> ExpressionStatement {
        todo!()
    }

    fn parse_prefix_expression(&mut self) -> Box<dyn Expression> {
        let token = self.cur_token();
        self.next();
        let right = self.parse_expression(precedence(token.clone()));
        Box::new(PrefixExpression { token, right })
    }

    fn parse_infix_expression(&mut self, left: Box<dyn Expression>) -> Box<dyn Expression> {
        let token = self.cur_token();
        self.next();
        let right = self.parse_expression(precedence(token.clone()));
        Box::new(InfixExpression { token, left, right })
    }

    fn parse_int_literal(&mut self) -> Box<dyn Expression> {
        let token = self.expect_int();
        Box::new(IntLiteral { token })
    }

    fn parse_ident(&mut self) -> Box<dyn Expression> {
        let token = self.expect_ident();
        Box::new(Identifier { token })
    }
}

#[cfg(test)]
mod test {
    use crate::ast::Parser;
    use crate::token::Lexer;

    #[test]
    fn test() {
        let input = "
        let ten = 10-10*10-10*9;
        let five = 5/5*10;
        let result = five+ten;
        ";
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        while !lexer.at_eof() {
            let tok = lexer.next_token();
            tokens.push(tok);
        }
        dbg!(&tokens);
        let mut parser = Parser::new(tokens);
        let prog = parser.parse_program();
        for stmt in prog.statements {
            println!("{}", stmt.to_string());
        }
    }
}
