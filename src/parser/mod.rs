mod lexer;
mod parser;

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::{input::Stream, Parser};

pub use self::lexer::Token;
pub use self::parser::{Expr, Statement};

pub fn parse(src: &str) -> Option<Vec<Statement>> {
    let (tokens, lex_errs) = lexer::lexer().parse(src).into_output_errors();
    for err in lex_errs {
        println!("{}", err);
    }

    let ast = if let Some(tokens) = tokens {
        let len = src.chars().count();
        let (ast, parse_errs) = parser::parser().parse(&tokens).into_output_errors();
        for err in parse_errs {
            println!("{}", err);
        }
        ast
    } else {
        None
    };

    /*let errors = lex_errs
        .into_iter()
        .map(|e| e.map(|c| c.to_string()))
        .chain(parse_errs.into_iter().map(|e| e.map(|tok| tok.to_string())));

    errors.for_each(|e| {
        let report = Report::build(ReportKind::Error, (), e.span().start);

        let report = match e.reason() {
            chumsky::error::RichReason::Unclosed { span, delimiter } => report
                .with_message(format!(
                    "Unclosed delimiter {}",
                    delimiter.fg(Color::Yellow)
                ))
                .with_label(
                    Label::new(span.clone())
                        .with_message(format!(
                            "Unclosed delimiter {}",
                            delimiter.fg(Color::Yellow)
                        ))
                        .with_color(Color::Yellow),
                )
                .with_label(
                    Label::new(e.span())
                        .with_message(format!(
                            "Must be closed before this {}",
                            e.found()
                                .unwrap_or(&"end of file".to_string())
                                .fg(Color::Red)
                        ))
                        .with_color(Color::Red),
                ),
            chumsky::error::RichReason::ExpectedFound { expected, found } => report
                .with_message(format!(
                    "{}, expected {}",
                    if e.found().is_some() {
                        "Unexpected token in input"
                    } else {
                        "Unexpected end of input"
                    },
                    if e.expected().len() == 0 {
                        "something else".to_string()
                    } else {
                        e.expected()
                            .map(|expected| match expected {
                                Some(expected) => expected.to_string(),
                                None => "end of input".to_string(),
                            })
                            .collect::<Vec<_>>()
                            .join(", ")
                    }
                ))
                .with_label(
                    Label::new(e.span())
                        .with_message(format!(
                            "Unexpected token {}",
                            e.found()
                                .unwrap_or(&"end of file".to_string())
                                .fg(Color::Red)
                        ))
                        .with_color(Color::Red),
                ),
            chumsky::error::RichReason::Custom(msg) => report.with_message(msg).with_label(
                Label::new(e.span())
                    .with_message(format!("{}", msg.fg(Color::Red)))
                    .with_color(Color::Red),
            ),
        };

        report.finish().print(Source::from(&src)).unwrap();
    });*/

    ast
}
