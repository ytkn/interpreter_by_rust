use crate::token::Token;

pub const LOWEST: i32 = 0;
const EQUALS: i32 = 1;
const LESSGREATER: i32 = 2;
const SUM: i32 = 3;
const PRODUCT: i32 = 4;
pub const PREFIX: i32 = 5;
const CALL: i32 = 6;
const INDEX: i32 = 7;

pub fn precedence(token: Token) -> i32 {
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