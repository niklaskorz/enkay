#[derive(Clone, Debug)]
pub enum Statement {
    Block(Vec<Statement>),
    If(Box<Expr>, Vec<Statement>, Option<Box<Statement>>),
    While(Box<Expr>, Vec<Statement>),
    FunctionDeclaration {
        name: String,
        params: Vec<Param>,
        return_type: String,
        body: Vec<Statement>,
    },
    Declaration(String, Box<Expr>),
    Assignment(String, Box<Expr>),
    Return(Option<Box<Expr>>),
    Continue,
    Break,
    Expr(Box<Expr>),
}

#[derive(Clone, Debug)]
pub enum Expr {
    Nil,
    Ident(String),
    Decimal(f64),
    Integer(i64),
    Boolean(bool),
    String(String),
    Array(Vec<Box<Expr>>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Function {
        params: Vec<Param>,
        return_type: String,
        body: Vec<Statement>,
    },
    PrefixOp(PrefixOp, Box<Expr>),
    SuffixOp(Box<Expr>, SuffixOp),
    BinaryOp(Box<Expr>, BinaryOp, Box<Expr>),
}

#[derive(Clone, Debug)]
pub struct Param {
    pub name: String,
    pub data_type: String,
}

#[derive(Clone, Debug)]
pub enum PrefixOp {
    LogicalNot,
    Plus,
    Minus,
}

#[derive(Clone, Debug)]
pub enum SuffixOp {
    Call(Vec<Box<Expr>>),
    Index(Box<Expr>),
}

#[derive(Clone, Debug)]
pub enum BinaryOp {
    Multiply,
    Divide,
    Plus,
    Minus,

    Equal,
    NotEqual,
    GreaterOrEqual,
    LessOrEqual,
    Greater,
    Less,

    LogicalAnd,
    LogicalOr,
}
