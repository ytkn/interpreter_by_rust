use interpreter::{ast::Parser, token::Lexer};
use std::io::Write;

fn main() {
    loop {
        print!(">> ");
        std::io::stdout().flush().unwrap();
        let mut s = String::new();
        std::io::stdin().read_line(&mut s).unwrap();
        if s.starts_with('q') {
            println!("bye!");
            break;
        }
        let input = s.as_str();
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        while !lexer.at_eof() {
            let tok = lexer.next_token();
            tokens.push(tok);
        }
        let mut parser = Parser::new(tokens);
        let prog = parser.parse_program();
        for stmt in prog.statements {
            println!("{}", stmt.to_string())
        }
    }
}
