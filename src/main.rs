use interpreter::{ast::Parser, token::Lexer};
use std::io::Write;

fn main() {
    loop {
        print!(">> ");
        std::io::stdout().flush().unwrap();
        let mut s = String::new();
        std::io::stdin().read_line(&mut s).unwrap();
        let input = s.as_str();
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        while !lexer.at_eof() {
            let tok = lexer.next_token();
            tokens.push(tok);
        }
        let mut parser = Parser::new(tokens);
        let result = parser.parse_program();
        match result {
            Ok(prog) => prog
                .statements
                .into_iter()
                .for_each(|stmt| println!("{}", stmt.to_string())),
            Err(err) => println!("{}", err.msg),
        }
    }
}
