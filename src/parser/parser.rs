use chumsky::prelude::*;

use super::lexer::Token;

#[derive(Clone, Debug)]
pub enum Statement {
    Block(Vec<Statement>),
    If(Expr, Vec<Statement>, Option<Box<Statement>>),
    While(Expr, Vec<Statement>),
    Declaration(String, Expr),
    Assignment(String, Expr),
    Return(Option<Expr>),
    Continue,
    Break,
    Expr(Expr),
}

pub fn parser() -> impl Parser<Token, Vec<Statement>, Error = Simple<Token>> {
    let ident = select! { Token::Identifier(value) => value.clone() };
    let stmt = recursive(|stmt| {
        let expr = expr(stmt.clone());
        let if_stmt = recursive(|if_stmt| {
            let else_branch = just(Token::Else).ignore_then(
                if_stmt.or(stmt
                    .clone()
                    .repeated()
                    .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
                    .map(Statement::Block)),
            );
            just(Token::If)
                .ignore_then(expr.clone())
                .then(
                    stmt.clone()
                        .repeated()
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
                    .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
            )
            .map(|(condition, statements)| Statement::While(condition, statements));

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
            .or(declaration_stmt)
            .or(assignment_stmt)
            .or(return_stmt)
            .or(continue_stmt)
            .or(break_stmt)
            .or(expr_stmt)
    });

    let program = stmt.repeated();
    program.then_ignore(end())
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

fn expr(
    stmt: impl Parser<Token, Statement, Error = Simple<Token>> + Clone + 'static,
) -> impl Parser<Token, Expr, Error = Simple<Token>> + Clone {
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
                    .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
            )
            .then(
                stmt.repeated()
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

        let prefix_op = just(Token::LogicalNot)
            .or(just(Token::Plus))
            .or(just(Token::Minus));

        let call = expr
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .delimited_by(just(Token::LeftParen), just(Token::RightParen))
            .map(|params| SuffixOp::Call(params));
        let index = expr
            .clone()
            .delimited_by(just(Token::LeftBracket), just(Token::RightBracket))
            .map(|val| SuffixOp::Index(Box::new(val)));
        let suffix_op = call.or(index);

        let factor = prefix_op
            .repeated()
            .then(
                value
                    .then(suffix_op.repeated())
                    .foldl(|lhs, rhs| Expr::SuffixOp(Box::new(lhs), rhs)),
            )
            .foldr(|lhs, rhs| Expr::PrefixOp(lhs, Box::new(rhs)));
        let addend = factor
            .clone()
            .then(
                just(Token::Multiply)
                    .or(just(Token::Divide))
                    .then(factor)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| Expr::BinaryOp(Box::new(lhs), op.clone(), Box::new(rhs)));
        let term = addend
            .clone()
            .then(
                just(Token::Plus)
                    .or(just(Token::Minus))
                    .then(addend)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| Expr::BinaryOp(Box::new(lhs), op.clone(), Box::new(rhs)));
        let comp = term
            .clone()
            .then(
                just(Token::Equal)
                    .or(just(Token::NotEqual))
                    .or(just(Token::GreaterOrEqual))
                    .or(just(Token::LessOrEqual))
                    .or(just(Token::Greater))
                    .or(just(Token::Less))
                    .then(term)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| Expr::BinaryOp(Box::new(lhs), op.clone(), Box::new(rhs)));
        let l_and = comp
            .clone()
            .then(just(Token::LogicalAnd).then(comp).repeated())
            .foldl(|lhs, (op, rhs)| Expr::BinaryOp(Box::new(lhs), op.clone(), Box::new(rhs)));
        let l_or = l_and
            .clone()
            .then(just(Token::LogicalOr).then(l_and).repeated())
            .foldl(|lhs, (op, rhs)| Expr::BinaryOp(Box::new(lhs), op.clone(), Box::new(rhs)));

        l_or
    })
}