pub mod ast;

use lalrpop_util::{ParseError, lalrpop_mod};

lalrpop_mod!(grammar, "parser/grammar.rs");

pub fn parse<'a>(input: &'a str) -> Result<Vec<ast::Statement>, ParseError<usize, grammar::Token<'a>, &'static str>> {
    let parser = grammar::BlockParser::new();
    parser.parse(input)
}
