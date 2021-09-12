extern crate clap;
use clap::{App, Arg};

use interpreter::environment::Environment;
use interpreter::formatter::run_formatter;
use interpreter::token::Token;
use interpreter::{parser::Parser, token::Lexer};
use std::fs::{self, OpenOptions};
use std::io::Write;

fn main() {
    let app = App::new("monkey")
        .version("0.1.0")
        .author("ytkn")
        .arg(Arg::with_name("file").help("file to read").required(false))
        .arg(
            Arg::with_name("format")
                .help("if on. runs formatter")
                .short("f")
                .long("format"),
        );
    let matches = app.get_matches();
    match matches.value_of("file") {
        Some(filename) => {
            let input = fs::read_to_string(filename).expect("failed to read");
            match Lexer::new(input.as_str()).tokenize() {
                Ok(tokens) => {
                    if matches.is_present("format") {
                        let formatted = run_formatter(tokens).unwrap();
                        write_content_to_file(formatted, filename);
                        println!("formatted {}", filename);
                    } else {
                        let mut environment = Environment::new();
                        parse_and_eval(tokens, &mut environment);
                    }
                }
                Err(err) => println!("{}", err.msg),
            }
        }
        None => run_interpreter(),
    };
}

fn write_content_to_file(content: String, filename: &str) {
    let mut file = OpenOptions::new().write(true).open(filename).unwrap();
    file.write_all(content.as_bytes()).unwrap()
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
