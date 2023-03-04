use chumsky::{prelude::*, recovery::skip_until, text::newline};

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
    Colon,              // :
    Arrow,              // ->
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
            Token::Colon => write!(f, ":"),
            Token::Arrow => write!(f, "->"),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),
            Token::LeftBracket => write!(f, "["),
            Token::RightBracket => write!(f, "]"),
            Token::Identifier(val) => write!(f, "{}", val),
            Token::Integer(val) => write!(f, "{}", val),
            Token::Decimal(val) => write!(f, "{}", val),
            Token::String(val) => write!(f, "\"{}\"", val),
            Token::Boolean(val) => write!(f, "{}", val),
        }
    }
}

pub type Span = std::ops::Range<usize>;

pub fn lexer<'a>() -> impl Parser<'a, &'a str, Vec<Token>, extra::Err<Rich<'a, &'a str>>> {
    let line_comment = just("//").then(skip_until(newline())).padded().to(());
    let block_comment = just("/*").then(skip_until(just("*/"))).padded().to(());
    let comment = line_comment.or(block_comment);
    keyword()
        .or(control_tokens())
        .or(operator())
        .or(literal())
        //.map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
        .collect::<Vec<_>>()
        .then_ignore(end())
}

fn keyword<'a>() -> impl Parser<'a, &'a str, Token, extra::Err<Rich<'a, &'a str>>> {
    text::keyword("return")
        .to(Token::Return)
        .or(text::keyword("continue").to(Token::Continue))
        .or(text::keyword("break").to(Token::Break))
        .or(text::keyword("func").to(Token::Func))
        .or(text::keyword("if").to(Token::If))
        .or(text::keyword("else").to(Token::Else))
        .or(text::keyword("while").to(Token::While))
}

fn operator<'a>() -> impl Parser<'a, &'a str, Token, extra::Err<Rich<'a, &'a str>>> {
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

fn control_tokens<'a>() -> impl Parser<'a, &'a str, Token, extra::Err<Rich<'a, &'a str>>> {
    just(";")
        .to(Token::Semicolon)
        .or(just(",").to(Token::Comma))
        .or(just(":").to(Token::Colon))
        .or(just("->").to(Token::Arrow))
        .or(just("(").to(Token::LeftParen))
        .or(just(")").to(Token::RightParen))
        .or(just("{").to(Token::LeftBrace))
        .or(just("}").to(Token::RightBrace))
        .or(just("[").to(Token::LeftBracket))
        .or(just("]").to(Token::RightBracket))
}

fn literal<'a>() -> impl Parser<'a, &'a str, Token, extra::Err<Rich<'a, &'a str>>> {
    let escape = just('\\')
        .then(choice((
            just('\\'),
            just('/'),
            just('"'),
            just('b').to('\x08'),
            just('f').to('\x0C'),
            just('n').to('\n'),
            just('r').to('\r'),
            just('t').to('\t'),
            just('u').ignore_then(text::digits(16).exactly(4).slice().validate(
                |digits, span, emitter| {
                    char::from_u32(u32::from_str_radix(digits, 16).unwrap()).unwrap_or_else(|| {
                        emitter.emit(Rich::custom(span, "invalid unicode character"));
                        '\u{FFFD}' // unicode replacement character
                    })
                },
            )),
        )))
        .ignored()
        .boxed();
    let string = none_of("\\\"")
        .ignored()
        .or(escape)
        .repeated()
        .slice()
        .map(ToString::to_string)
        .delimited_by(just('"'), just('"'))
        .map(Token::String);
    let decimal = text::int(10)
        .then(just('.'))
        .then(text::digits(10).slice())
        .map_slice(ToString::to_string)
        .map(Token::Decimal);
    let integer = text::int(10).map(ToString::to_string).map(Token::Integer);
    let boolean = text::keyword("true")
        .to(true)
        .or(text::keyword("false").to(false))
        .map(Token::Boolean);
    let nil = text::keyword("nil").to(Token::Nil);
    let ident = text::ident()
        .map(ToString::to_string)
        .map(Token::Identifier);
    string.or(decimal).or(integer).or(boolean).or(nil).or(ident)
}
