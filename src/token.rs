use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    PLUS,
    MINUS,
    ASTERISK,
    SLASH,

    LT,
    GT,
    EQ,
    NE,

    ASSIGN,
    BANG,
    SEMICOLON,
    LPAREN,
    RPAREN,
    COMMA,
    LBRACE,
    RBRACE,

    LET,
    FUNCTION,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,

    IDENT(String),
    INT(i32),
    EOF,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::IDENT(_) => write!(f, "ident"),
            Token::INT(_) => write!(f, "int"),
            Token::PLUS => write!(f, "+"),
            Token::MINUS => write!(f, "-"),
            Token::ASTERISK => write!(f, "*"),
            Token::SLASH => write!(f, "/"),
            Token::LT => write!(f, "<"),
            Token::GT => write!(f, ">"),
            Token::EQ => write!(f, "=="),
            Token::NE => write!(f, "!="),
            Token::ASSIGN => todo!(),
            Token::BANG => todo!(),
            Token::SEMICOLON => write!(f, ";"),
            Token::LPAREN => todo!(),
            Token::RPAREN => todo!(),
            Token::COMMA => todo!(),
            Token::LBRACE => todo!(),
            Token::RBRACE => todo!(),
            Token::LET => write!(f, "let"),
            Token::FUNCTION => todo!(),
            Token::TRUE => todo!(),
            Token::FALSE => todo!(),
            Token::IF => todo!(),
            Token::ELSE => todo!(),
            Token::RETURN => todo!(),
            Token::EOF => todo!(),
        }
    }
}

pub struct Lexer {
    input: Vec<char>,
    pos: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let chars: Vec<char> = input.chars().collect();
        Lexer {
            input: chars,
            pos: 0,
        }
    }
    fn cur(&self) -> char {
        if self.pos == self.input.len() {
            '\u{0}'
        } else {
            self.input[self.pos]
        }
    }

    fn peek(&self) -> char {
        if self.pos + 1 >= self.input.len() {
            '\u{0}'
        } else {
            self.input[self.pos + 1]
        }
    }

    fn next(&mut self) {
        if self.pos == self.input.len() {
            return;
        }
        self.pos += 1;
    }

    pub fn next_token(&mut self) -> Token {
        while self.cur().is_whitespace() {
            self.next();
        }
        if self.at_eof() {
            return Token::EOF;
        }
        if is_letter(self.cur()) {
            let ident = self.read_ident();
            return match &*ident {
                "fn" => Token::FUNCTION,
                "let" => Token::LET,
                "true" => Token::TRUE,
                "false" => Token::FALSE,
                "if" => Token::IF,
                "else" => Token::ELSE,
                "return" => Token::RETURN,
                _ => Token::IDENT(ident),
            };
        }
        if self.cur().is_digit(10) {
            return Token::INT(self.read_number());
        }
        let tok = match self.cur() {
            '=' => match self.peek() {
                '=' => {
                    self.next();
                    Token::EQ
                }
                _ => Token::ASSIGN,
            },
            ';' => Token::SEMICOLON,
            '(' => Token::LPAREN,
            ')' => Token::RPAREN,
            '{' => Token::LBRACE,
            '}' => Token::RBRACE,
            ',' => Token::COMMA,
            '+' => Token::PLUS,
            '-' => Token::MINUS,
            '*' => Token::ASTERISK,
            '/' => Token::SLASH,
            '<' => Token::LT,
            '>' => Token::GT,
            '!' => match self.peek() {
                '=' => {
                    self.next();
                    Token::NE
                }
                _ => Token::BANG,
            },
            x => unreachable!(format!("unreachable: {}", x)),
        };
        self.next();
        tok
    }

    fn read_ident(&mut self) -> String {
        if !is_letter(self.cur()) {
            panic!("initial char is not a letter");
        }
        let l = self.pos;
        while is_letter(self.cur()) {
            self.next();
        }
        self.input[l..self.pos].into_iter().collect()
    }

    fn read_number(&mut self) -> i32 {
        if !self.cur().is_digit(10) {
            panic!("initial char is not a digit");
        }
        let l = self.pos;
        while self.cur().is_digit(10) {
            self.next();
        }
        self.input[l..self.pos]
            .into_iter()
            .collect::<String>()
            .parse()
            .unwrap() // 長すぎると死ぬ気がする
    }

    pub fn at_eof(&self) -> bool {
        self.pos == self.input.len()
    }
}

fn is_letter(c: char) -> bool {
    if 'a' <= c && c <= 'z' {
        return true;
    }
    if 'A' <= c && c <= 'Z' {
        return true;
    }
    if c == '_' {
        return true;
    }
    false
}

#[cfg(test)]
mod test {
    use crate::token::is_letter;
    use crate::token::Lexer;
    use crate::token::Token;

    #[test]
    fn test_is_letter() {
        assert!(is_letter('c'));
        assert!(is_letter('A'));
        assert!(is_letter('X'));
        assert!(is_letter('_'));
        assert!(!is_letter('='));
        assert!(!is_letter('{'));
        assert!(!is_letter('1'));
    }

    #[test]
    fn test_lexer() {
        let input = "
        let ten = 10;
        let five = 5;

        let add = fn(x, y) {
            x+y;
        };
        let result = add(x, y);
        if(x != 100-20) {
            return x/100;
        }
        if(x+y == 2) {
            return result;
        }else{
            return result*2;
        }
        ";
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        while !lexer.at_eof() {
            let tok = lexer.next_token();
            tokens.push(tok);
        }
        dbg!(&tokens);
        let expected = [
            Token::LET,
            Token::IDENT("ten".to_string()),
            Token::ASSIGN,
            Token::INT(10),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("five".to_string()),
            Token::ASSIGN,
            Token::INT(5),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("add".to_string()),
            Token::ASSIGN,
            Token::FUNCTION,
            Token::LPAREN,
            Token::IDENT("x".to_string()),
            Token::COMMA,
            Token::IDENT("y".to_string()),
            Token::RPAREN,
            Token::LBRACE,
            Token::IDENT("x".to_string()),
            Token::PLUS,
            Token::IDENT("y".to_string()),
            Token::SEMICOLON,
            Token::RBRACE,
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("result".to_string()),
            Token::ASSIGN,
            Token::IDENT("add".to_string()),
            Token::LPAREN,
            Token::IDENT("x".to_string()),
            Token::COMMA,
            Token::IDENT("y".to_string()),
            Token::RPAREN,
            Token::SEMICOLON,
            Token::EOF,
        ];
        assert_eq!(expected.len(), tokens.len());
        for i in 0..expected.len() {
            assert_eq!(expected[i], tokens[i]);
        }
    }
}
