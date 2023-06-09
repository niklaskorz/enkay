mod lexer;
mod parser;

use std::ops::Range;

use ariadne::{sources, Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::error::RichReason;
use chumsky::input::{Input, Stream};
use chumsky::prelude::Rich;
use chumsky::Parser;

pub use self::lexer::Token;
pub use self::parser::{Expr, Statement};

struct SourceSpan {
    file_name: String,
    start: usize,
    end: usize,
}

impl ariadne::Span for SourceSpan {
    type SourceId = String;

    fn source(&self) -> &Self::SourceId {
        &self.file_name
    }

    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }
}

fn create_error_report<'a, T>(e: Rich<'a, T>) -> Report<SourceSpan>
where
    T: Input<'a>,
    T::Span: Clone + Into<Range<usize>>,
    T::Token: std::fmt::Display,
{
    let range: Range<usize> = e.span().into();
    let span = SourceSpan {
        file_name: "input".to_string(),
        start: range.start,
        end: range.end,
    };
    let report = Report::build(ReportKind::Error, "input", span.start);
    let report = match e.reason() {
        RichReason::ExpectedFound { expected, found } => report
            .with_message(format!(
                "{}, expected {}",
                if found.is_some() {
                    "Unexpected token in input"
                } else {
                    "Unexpected end of input"
                },
                if expected.len() == 0 {
                    "something else".to_string()
                } else {
                    expected
                        .into_iter()
                        .map(|expected| match expected {
                            Some(expected) => expected.to_string(),
                            None => "end of input".to_string(),
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                }
            ))
            .with_label(
                Label::new(span)
                    .with_message(format!(
                        "Unexpected token {}",
                        found
                            .as_ref()
                            .map(|found| found.to_string())
                            .unwrap_or("end of file".to_string())
                            .fg(Color::Red)
                    ))
                    .with_color(Color::Red),
            ),
        RichReason::Custom(msg) => report.with_message(msg).with_label(
            Label::new(span)
                .with_message(format!("{}", msg.fg(Color::Red)))
                .with_color(Color::Red),
        ),
        RichReason::Many(_) => todo!(),
    };
    report.finish()
}

pub fn parse(src: &str) -> Option<Vec<Statement>> {
    let files = vec![("input".to_string(), src)];
    let (tokens, lex_errs) = lexer::lexer().parse(src).into_output_errors();
    for err in lex_errs {
        let files = sources(files.clone());
        create_error_report(err).print(files).unwrap();
    }

    let ast = if let Some(tokens) = tokens {
        let len = src.chars().count();
        let (ast, parse_errs) = parser::parser()
            .parse(Stream::from_iter(tokens.into_iter()).spanned(len..len + 1))
            .into_output_errors();
        for err in parse_errs {
            let files = sources(files.clone());
            create_error_report(err).print(files).unwrap();
        }
        ast
    } else {
        None
    };

    ast
}
