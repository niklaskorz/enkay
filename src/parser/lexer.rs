use chumsky::{prelude::*, text::newline};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Return,             // return
    Continue,           // continue
    Break,              // break
    Func,               // func
    If,                 // if
    Else,               // else
    While,              // while
    Nil,                // nil
    Declare,            // :=
    Assign,             // =
    Multiply,           // *
    Divide,             // /
    Plus,               // +
    Minus,              // -
    LogicalNot,         // !
    LogicalOr,          // ||
    LogicalAnd,         // &&
    Equal,              // ==
    NotEqual,           // !=
    LessOrEqual,        // <=
    GreaterOrEqual,     // >=
    Less,               // <
    Greater,            // >
    Semicolon,          // ;
    Comma,              // ,
    LeftParen,          // (
    RightParen,         // )
    LeftBrace,          // {
    RightBrace,         // }
    LeftBracket,        // [
    RightBracket,       // ]
    Identifier(String), // Unicode letter followed by unicode letters or digits
    Integer(String),    // Digits
    Decimal(String),    // Real numbers
    String(String),     // Arbitrary characters enclosed by quotation marks
    Boolean(bool),      // true or false
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Token::Return => write!(f, "return"),
            Token::Continue => write!(f, "continue"),
            Token::Break => write!(f, "break"),
            Token::Func => write!(f, "func"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::While => write!(f, "while"),
            Token::Nil => write!(f, "nil"),
            Token::Declare => write!(f, ":="),
            Token::Assign => write!(f, "="),
            Token::Multiply => write!(f, "*"),
            Token::Divide => write!(f, "/"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::LogicalNot => write!(f, "!"),
            Token::LogicalOr => write!(f, "||"),
            Token::LogicalAnd => write!(f, "&&"),
            Token::Equal => write!(f, "=="),
            Token::NotEqual => write!(f, "!="),
            Token::LessOrEqual => write!(f, "<="),
            Token::GreaterOrEqual => write!(f, ">="),
            Token::Less => write!(f, "<"),
            Token::Greater => write!(f, ">"),
            Token::Semicolon => write!(f, ";"),
            Token::Comma => write!(f, ","),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),
            Token::LeftBracket => write!(f, "["),
            Token::RightBracket => write!(f, "]"),
            Token::Identifier(val) => write!(f, "{}", val),
            Token::Integer(val) => write!(f, "{}", val),
            Token::Decimal(val) => write!(f, "{}", val),
            Token::String(val) => write!(f, "{}", val),
            Token::Boolean(val) => write!(f, "{}", val),
        }
    }
}

pub type Span = std::ops::Range<usize>;

pub fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    let line_comment = just("//").then(take_until(newline())).padded().to(());
    let block_comment = just("/*").then(take_until(just("*/"))).padded().to(());
    let comment = line_comment.or(block_comment);
    keyword()
        .or(operator())
        .or(control_tokens())
        .or(literal())
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
        .then_ignore(end())
}

fn keyword() -> impl Parser<char, Token, Error = Simple<char>> {
    just("return")
        .to(Token::Return)
        .or(just("continue").to(Token::Continue))
        .or(just("break").to(Token::Break))
        .or(just("func").to(Token::Func))
        .or(just("if").to(Token::If))
        .or(just("else").to(Token::Else))
        .or(just("while").to(Token::While))
}

fn operator() -> impl Parser<char, Token, Error = Simple<char>> {
    just(":=")
        .to(Token::Declare)
        .or(just("==").to(Token::Equal))
        .or(just("!=").to(Token::NotEqual))
        .or(just("<=").to(Token::LessOrEqual))
        .or(just(">=").to(Token::GreaterOrEqual))
        .or(just("||").to(Token::LogicalOr))
        .or(just("&&").to(Token::LogicalAnd))
        .or(just('<').to(Token::Less))
        .or(just('<').to(Token::Greater))
        .or(just('=').to(Token::Assign))
        .or(just('+').to(Token::Plus))
        .or(just('-').to(Token::Minus))
        .or(just('*').to(Token::Multiply))
        .or(just('/').to(Token::Divide))
        .or(just('!').to(Token::LogicalNot))
}

fn control_tokens() -> impl Parser<char, Token, Error = Simple<char>> {
    just(";")
        .to(Token::Semicolon)
        .or(just(",").to(Token::Comma))
        .or(just("(").to(Token::LeftParen))
        .or(just(")").to(Token::RightParen))
        .or(just("{").to(Token::LeftBrace))
        .or(just("}").to(Token::RightBrace))
        .or(just("[").to(Token::LeftBracket))
        .or(just("]").to(Token::RightBracket))
}

fn literal() -> impl Parser<char, Token, Error = Simple<char>> {
    let escape = just('\\').ignore_then(
        just('\\')
            .or(just('/'))
            .or(just('"'))
            .or(just('b').to('\x08'))
            .or(just('f').to('\x0C'))
            .or(just('n').to('\n'))
            .or(just('r').to('\r'))
            .or(just('t').to('\t'))
            .or(just('u').ignore_then(
                filter(|c: &char| c.is_digit(16))
                    .repeated()
                    .exactly(4)
                    .collect::<String>()
                    .validate(|digits, span, emit| {
                        char::from_u32(u32::from_str_radix(&digits, 16).unwrap()).unwrap_or_else(
                            || {
                                emit(Simple::custom(span, "invalid unicode character"));
                                '\u{FFFD}' // unicode replacement character
                            },
                        )
                    }),
            )),
    );
    let string = just('"')
        .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape).repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::String);
    let decimal = text::int(10)
        .chain::<char, _, _>(just('.'))
        .chain::<char, _, _>(text::digits(10))
        .collect::<String>()
        .map(Token::Decimal);
    let integer = text::int(10).map(Token::Integer);
    let boolean = just("true")
        .to(true)
        .or(just("false").to(false))
        .map(Token::Boolean);
    let nil = just("nil").to(Token::Nil);
    let ident = text::ident().map(Token::Identifier);
    string.or(decimal).or(integer).or(boolean).or(nil).or(ident)
}
