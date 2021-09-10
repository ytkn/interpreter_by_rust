use interpreter::environment::Environment;
use interpreter::token::Token;
use interpreter::{parser::Parser, token::Lexer};
use std::io::Write;

fn parse_and_eval(tokens: Vec<Token>, env: &mut Environment) {
    let mut parser = Parser::new(tokens);
    let result = parser.parse_program();
    match result {
        Ok(prog) => prog.statements.into_iter().for_each(|stmt| {
            let to_show = match stmt.eval(env) {
                Ok(obj) => obj.inspect(),
                Err(err) => err.msg,
            };
            println!("{:?}", to_show)
        }),
        Err(err) => println!("{}", err.msg),
    }
}

fn main() {
    let mut env = Environment::new();
    loop {
        print!(">> ");
        std::io::stdout().flush().unwrap();
        let mut s = String::new();
        std::io::stdin().read_line(&mut s).unwrap();
        if s.eq("quit()\n") {
            println!("bye!");
            break;
        }
        if s.eq("values()\n") {
            println!("{}", env.inspect());
            continue;
        }
        let input = s.as_str();
        let mut lexer = Lexer::new(input);
        let tokenize_result = lexer.tokenize();
        match tokenize_result {
            Ok(tokens) => parse_and_eval(tokens, &mut env),
            Err(err) => println!("{}", err.msg),
        }
    }
}
