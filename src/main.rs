use interpreter::environment::Environment;
use interpreter::token::Token;
use interpreter::{parser::Parser, token::Lexer};
use std::io::Write;
use std::{env, fs};

fn main() {
    let args: Vec<String> = env::args().collect();
    if *&args.last().unwrap().ends_with(".monkey") {
        let filename = &args.last().unwrap();
        let input = fs::read_to_string(filename).expect("failed to read");
        let mut lexer = Lexer::new(input.as_str());
        let tokenize_result = lexer.tokenize();
        match tokenize_result {
            Ok(tokens) => {
                let mut environment = Environment::new();
                parse_and_eval(tokens, &mut environment);
            }
            Err(err) => println!("{}", err.msg),
        }
    } else {
        run_interpreter();
    }
}

fn parse_and_eval(tokens: Vec<Token>, environment: &mut Environment) {
    let mut parser = Parser::new(tokens);
    let result = parser.parse_program();
    match result {
        Ok(prog) => prog.statements.into_iter().for_each(|stmt| {
            match stmt.eval(environment) {
                Ok(_) => {}
                Err(err) => println!("{:?}", err.msg),
            };
        }),
        Err(err) => println!("{}", err.msg),
    }
}

fn run_interpreter() {
    let mut environment = Environment::new();
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
            println!("{}", environment.inspect());
            continue;
        }
        let input = s.as_str();
        let mut lexer = Lexer::new(input);
        let tokenize_result = lexer.tokenize();
        match tokenize_result {
            Ok(tokens) => parse_and_eval(tokens, &mut environment),
            Err(err) => println!("{}", err.msg),
        }
    }
}
