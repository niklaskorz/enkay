use chumsky::prelude::*;

use super::lexer::Token;

#[derive(Clone, Debug)]
pub struct Param {
    pub name: String,
    pub data_type: String,
}

#[derive(Clone, Debug)]
pub enum Statement {
    Block(Vec<Statement>),
    If(Expr, Vec<Statement>, Option<Box<Statement>>),
    While(Expr, Vec<Statement>),
    FunctionDeclaration {
        name: String,
        params: Vec<Param>,
        return_type: String,
        body: Vec<Statement>,
    },
    Declaration(String, Expr),
    Assignment(String, Expr),
    Return(Option<Expr>),
    Continue,
    Break,
    Expr(Expr),
}

pub fn parser<'a>(
) -> impl Parser<'a, &'a [Token], Vec<Statement>, extra::Err<Rich<'a, &'a [Token]>>> {
    let ident = select! { Token::Identifier(value) => value.clone() };
    let stmt = recursive(|stmt| {
        let expr = expr(stmt.clone());
        let if_stmt = recursive(|if_stmt| {
            let else_branch = just(Token::Else).ignore_then(
                if_stmt.or(stmt
                    .clone()
                    .repeated()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
                    .map(Statement::Block)),
            );
            just(Token::If)
                .ignore_then(expr.clone())
                .then(
                    stmt.clone()
                        .repeated()
                        .collect::<Vec<_>>()
                        .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
                )
                .then(else_branch.or_not())
                .map(|((condition, statements), branch)| {
                    Statement::If(condition, statements, branch.map(Box::new))
                })
        });
        let while_stmt = just(Token::While)
            .ignore_then(expr.clone())
            .then(
                stmt.clone()
                    .repeated()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
            )
            .map(|(condition, statements)| Statement::While(condition, statements));

        let function_declaration_stmt = just(Token::Func)
            .ignore_then(ident)
            .then(
                ident
                    .then_ignore(just(Token::Colon))
                    .then(ident)
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
            )
            .then_ignore(just(Token::Arrow))
            .then(ident)
            .then(
                stmt.clone()
                    .repeated()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
            )
            .map(
                |(((name, params), return_type), body)| Statement::FunctionDeclaration {
                    name,
                    params: params
                        .into_iter()
                        .map(|(name, data_type)| Param { name, data_type })
                        .collect(),
                    return_type,
                    body,
                },
            );

        let declaration_stmt = ident
            .then_ignore(just(Token::Declare))
            .then(expr.clone())
            .then_ignore(just(Token::Semicolon))
            .map(|(ident, value)| Statement::Declaration(ident, value));
        let assignment_stmt = ident
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .then_ignore(just(Token::Semicolon))
            .map(|(ident, value)| Statement::Assignment(ident, value));

        let return_stmt = just(Token::Return)
            .ignore_then(expr.clone().or_not())
            .then_ignore(just(Token::Semicolon))
            .map(Statement::Return);
        let continue_stmt = just(Token::Continue)
            .to(Statement::Continue)
            .then_ignore(just(Token::Semicolon));
        let break_stmt = just(Token::Break)
            .to(Statement::Break)
            .then_ignore(just(Token::Semicolon));

        let expr_stmt = expr
            .then_ignore(just(Token::Semicolon))
            .map(Statement::Expr);

        if_stmt
            .or(while_stmt)
            .or(function_declaration_stmt)
            .or(declaration_stmt)
            .or(assignment_stmt)
            .or(return_stmt)
            .or(continue_stmt)
            .or(break_stmt)
            .or(expr_stmt)
    });

    stmt.repeated().collect::<Vec<_>>().then_ignore(end())
}

#[derive(Clone, Debug)]
pub enum SuffixOp {
    Call(Vec<Expr>),
    Index(Box<Expr>),
}

#[derive(Clone, Debug)]
pub enum Expr {
    Nil,
    Ident(String),
    Decimal(f64),
    Integer(i64),
    Boolean(bool),
    String(String),
    Array(Vec<Expr>),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    Function(Vec<String>, Vec<Statement>),
    PrefixOp(Token, Box<Expr>),
    SuffixOp(Box<Expr>, SuffixOp),
    BinaryOp(Box<Expr>, Token, Box<Expr>),
}

fn expr<'a>(
    stmt: impl Parser<'a, &'a [Token], Statement, extra::Err<Rich<'a, &'a [Token]>>> + Clone + 'a,
) -> impl Parser<'a, &'a [Token], Expr, extra::Err<Rich<'a, &'a [Token]>>> + Clone {
    let string = select! { Token::String(value) => value.clone() }.map(Expr::String);
    let decimal = select! { Token::Decimal(value) => value.parse() }
        .unwrapped()
        .map(Expr::Decimal);
    let integer = select! { Token::Integer(value) => value.parse() }
        .unwrapped()
        .map(Expr::Integer);
    let boolean = select! { Token::Boolean(value) => value }.map(Expr::Boolean);
    let nil = just(Token::Nil).to(Expr::Nil);
    let ident = select! { Token::Identifier(value) => value.clone() };
    let expr_ident = ident.map(Expr::Ident);

    recursive(move |expr| {
        let nested_expr = expr
            .clone()
            .delimited_by(just(Token::LeftParen), just(Token::RightParen));
        let array = expr
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LeftBracket), just(Token::RightBracket))
            .map(|entries| Expr::Array(entries));
        let if_expr = recursive(|if_expr| {
            let else_branch = just(Token::Else).ignore_then(
                if_expr.or(expr
                    .clone()
                    .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))),
            );
            just(Token::If)
                .ignore_then(expr.clone())
                .then(
                    expr.clone()
                        .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
                )
                .then(else_branch.or_not())
                .map(|((cond, val), branch)| {
                    Expr::If(Box::new(cond), Box::new(val), branch.map(Box::new))
                })
        });
        let function = just(Token::Func)
            .ignore_then(
                ident
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
            )
            .then(
                stmt.repeated()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
            )
            .map(|(params, statements)| Expr::Function(params, statements));
        let value = nested_expr
            .or(array)
            .or(if_expr)
            .or(function)
            .or(string)
            .or(decimal)
            .or(integer)
            .or(boolean)
            .or(nil)
            .or(expr_ident);

        let prefix_op = one_of(&[Token::LogicalNot, Token::Plus, Token::Minus]);

        let call = expr
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LeftParen), just(Token::RightParen))
            .map(|params| SuffixOp::Call(params));
        let index = expr
            .clone()
            .delimited_by(just(Token::LeftBracket), just(Token::RightBracket))
            .map(|val| SuffixOp::Index(Box::new(val)));
        let suffix_op = call.or(index);

        let factor = prefix_op.repeated().foldr(
            value.foldl(suffix_op.repeated(), |lhs, rhs| {
                Expr::SuffixOp(Box::new(lhs), rhs)
            }),
            |lhs, rhs| Expr::PrefixOp(lhs, Box::new(rhs)),
        );
        let addend = factor.clone().foldl(
            one_of(&[Token::Multiply, Token::Divide])
                .then(factor)
                .repeated(),
            |lhs, (op, rhs)| Expr::BinaryOp(Box::new(lhs), op.clone(), Box::new(rhs)),
        );
        let term = addend.clone().foldl(
            one_of(&[Token::Plus, Token::Minus]).then(addend).repeated(),
            |lhs, (op, rhs)| Expr::BinaryOp(Box::new(lhs), op.clone(), Box::new(rhs)),
        );
        let comp = term.clone().foldl(
            one_of(&[
                Token::Equal,
                Token::NotEqual,
                Token::GreaterOrEqual,
                Token::LessOrEqual,
                Token::Greater,
                Token::Less,
            ])
            .then(term)
            .repeated(),
            |lhs, (op, rhs)| Expr::BinaryOp(Box::new(lhs), op.clone(), Box::new(rhs)),
        );
        let l_and = comp.clone().foldl(
            just(Token::LogicalAnd).then(comp).repeated(),
            |lhs, (op, rhs)| Expr::BinaryOp(Box::new(lhs), op.clone(), Box::new(rhs)),
        );
        let l_or = l_and.clone().foldl(
            just(Token::LogicalOr).then(l_and).repeated(),
            |lhs, (op, rhs)| Expr::BinaryOp(Box::new(lhs), op.clone(), Box::new(rhs)),
        );

        l_or
    })
}
