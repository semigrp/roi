pub enum Expr {
    Number(f64),
    Boolean(bool),
    Ident(String),
    BinaryOp(Box<Expr>, BinOp, Box<Expr>),
    UnaryOp(UnOp, Box<Expr>),
    FnCall(String, Vec<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    While(Box<Expr>, Box<Expr>),
}

pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

pub enum UnOp {
    Neg,
    Not,
}

pub enum Stmt {
    Let(String, Expr),
    Expr(Expr),
    FnDef(String, Vec<String>, Box<Expr>),
    Return(Expr),
}

pub struct Program {
    pub stmts: Vec<Stmt>,
}