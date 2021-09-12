use std::{ops::Add, rc::Rc};

use crate::{
    ast::*,
    parser::{ParseError, Parser},
    precedence::{precedence, LOWEST, PREFIX},
    token::Token,
};

pub trait Formattable {
    fn format(&self, indent: u8, indent_size: u8, parent_precedence: i32) -> String;
}

impl Formattable for Identifier {
    fn format(&self, _indent: u8, _indent_size: u8, _parent_precedence: i32) -> String {
        match &self.token {
            Token::IDENT(name) => name.to_string(),
            _ => unreachable!(),
        }
    }
}

impl Formattable for IntLiteral {
    fn format(&self, _indent: u8, _indent_size: u8, _parent_precedence: i32) -> String {
        match self.token {
            Token::INT(x) => x.to_string(),
            _ => unreachable!(),
        }
    }
}

impl Formattable for StringLiteral {
    fn format(&self, _indent: u8, _indent_size: u8, _parent_precedence: i32) -> String {
        match &self.token {
            Token::STRING(name) => format!("\"{}\"", name.to_string()),
            _ => unreachable!(),
        }
    }
}

fn format_pairs(
    pairs: &Vec<(Rc<dyn Expression>, Rc<dyn Expression>)>,
    indent: u8,
    indent_size: u8,
) -> String {
    pairs
        .into_iter()
        .map(|(k, v)| {
            format!(
                "{}, {}",
                k.format(indent, indent_size, LOWEST),
                v.format(indent, indent_size, LOWEST)
            )
        })
        .collect::<Vec<String>>()
        .join(", ")
}

impl Formattable for DictLiteral {
    fn format(&self, indent: u8, indent_size: u8, _parent_precedence: i32) -> String {
        format!("{{{}}}", format_pairs(&self.pairs, indent, indent_size))
    }
}

fn format_params(params: &Vec<Rc<Identifier>>) -> String {
    params
        .into_iter()
        .map(|p| p.format(0, 0, LOWEST))
        .collect::<Vec<String>>()
        .join(", ")
}

impl Formattable for FunctionLiteral {
    fn format(&self, indent: u8, indent_size: u8, _parent_precedence: i32) -> String {
        format!(
            "fn({}) {}",
            format_params(&self.params),
            self.body.format(indent, indent_size, LOWEST)
        )
    }
}

fn format_expressions(expressions: &Vec<Rc<dyn Expression>>) -> String {
    expressions
        .into_iter()
        .map(|p| p.format(0, 0, LOWEST))
        .collect::<Vec<String>>()
        .join(", ")
}

impl Formattable for FunctionCall {
    fn format(&self, _indent: u8, _indent_size: u8, _parent_precedence: i32) -> String {
        format!(
            "{}({})",
            self.function.format(0, 0, LOWEST),
            format_expressions(&self.args)
        )
    }
}

impl Formattable for Boolean {
    fn format(&self, _indent: u8, _indent_sizee: u8, _parent_precedence: i32) -> String {
        match &self.token {
            Token::TRUE => "true".to_string(),
            Token::FALSE => "false".to_string(),
            _ => unreachable!(),
        }
    }
}

impl Formattable for ArrayLiteral {
    fn format(&self, _indent: u8, _indent_size: u8, _parent_precedence: i32) -> String {
        format!("[{}]", format_expressions(&self.elements))
    }
}

impl Formattable for IndexExpression {
    fn format(&self, _indent: u8, indent_size: u8, _parent_precedence: i32) -> String {
        format!(
            "{}[{}]",
            self.left.format(0, indent_size, LOWEST),
            self.index.format(0, indent_size, LOWEST)
        )
    }
}

fn make_indent(spaces: u8) -> String {
    (0..spaces)
        .map(|_| " ".to_string())
        .collect::<Vec<String>>()
        .join("")
}

impl Formattable for LetStatement {
    fn format(&self, indent: u8, indent_size: u8, _parent_precedence: i32) -> String {
        format!(
            "let {} = {}",
            self.ident.format(indent, indent_size, LOWEST),
            self.value.format(indent, indent_size, LOWEST)
        )
    }
}

impl Formattable for ReturnStatement {
    fn format(&self, indent: u8, indent_size: u8, _parent_precedence: i32) -> String {
        format!(
            "return {}",
            self.return_value.format(indent, indent_size, LOWEST),
        )
    }
}

impl Formattable for ExpressionStatement {
    fn format(&self, indent: u8, indent_size: u8, _parent_precedence: i32) -> String {
        self.value.format(indent, indent_size, LOWEST)
    }
}

impl Formattable for IfExpression {
    fn format(&self, indent: u8, indent_size: u8, _parent_precedence: i32) -> String {
        match &self.alternative {
            Some(statements) => format!(
                "if ({}) {} else {}",
                self.condition.format(indent, indent_size, LOWEST),
                self.consequense.format(indent, indent_size, LOWEST),
                statements.format(indent, indent_size, LOWEST)
            ),
            None => format!(
                "if ({}) {}",
                self.condition.format(indent, indent_size, LOWEST),
                self.consequense.format(indent, indent_size, LOWEST)
            ),
        }
    }
}

fn format_statements(statements: &Vec<Rc<dyn Statement>>, indent: u8, indent_size: u8) -> String {
    statements
        .into_iter()
        .map(|s| {
            format!(
                "{}{}",
                make_indent(indent * indent_size),
                s.format(indent, indent_size, LOWEST)
            )
        })
        .collect::<Vec<String>>()
        .join(";\n")
        .add(";\n")
}

impl Formattable for BlockStatement {
    fn format(&self, indent: u8, indent_size: u8, _parent_precedence: i32) -> String {
        format!(
            "{{\n{}{}}}",
            format_statements(&self.statements, indent + 1, indent_size),
            make_indent(indent_size * indent),
        )
    }
}

fn operator_to_string(ope: Token) -> String {
    match ope {
        Token::PLUS => "+",
        Token::MINUS => "-",
        Token::ASTERISK => "*",
        Token::SLASH => "/",
        Token::LT => "<",
        Token::GT => ">",
        Token::EQ => "==",
        Token::NE => "!=",
        Token::ASSIGN => "=",
        Token::BANG => "!",
        _ => unreachable!(),
    }
    .to_string()
}

impl Formattable for PrefixExpression {
    fn format(&self, indent: u8, indent_size: u8, parent_precedence: i32) -> String {
        let cur_precedence = PREFIX;
        let contet = format!(
            "{}{}",
            operator_to_string(self.token.clone()),
            self.right.format(indent, indent_size, cur_precedence),
        );
        if parent_precedence > cur_precedence {
            format!("({})", contet)
        } else {
            contet
        }
    }
}

impl Formattable for InfixExpression {
    fn format(&self, indent: u8, indent_size: u8, parent_precedence: i32) -> String {
        let cur_precedence = precedence(self.token.clone());
        let contet = format!(
            "{} {} {}",
            self.left.format(indent, indent_size, cur_precedence),
            operator_to_string(self.token.clone()),
            self.right.format(indent, indent_size, cur_precedence + 1),
        );
        if parent_precedence > cur_precedence {
            format!("({})", contet)
        } else {
            contet
        }
    }
}

impl Formattable for Program {
    fn format(&self, indent: u8, indent_size: u8, _parent_precedence: i32) -> String {
        format_statements(&self.statements, indent, indent_size)
    }
}

pub fn run_formatter(tokens: Vec<Token>) -> Result<String, ParseError> {
    let mut parser = Parser::new(tokens);
    let formatted = parser.parse_program()?.format(0, 4, LOWEST);
    Ok(formatted)
}

#[cfg(test)]
mod test {
    use super::Formattable;
    use crate::{parser::Parser, precedence::LOWEST, token::Lexer};

    #[test]
    fn test_format() {
        let input = "let add = fn(n){ if(n != 10){ let x = n != 10*2; return x }else{ return (n+10)*(10+19) } }";
        let expected = "let add = fn(n) {\n    if (n != 10) {\n        let x = n != 10 * 2;\n        return x;\n    } else {\n        return (n + 10) * (10 + 19);\n    };\n};\n";
        run(input, expected)
    }

    fn run(input: &str, expected: &str) {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let prog = parser.parse_program().unwrap();
        let formatted = prog.format(0, 4, LOWEST);
        assert_eq!(formatted, expected)
    }
}
