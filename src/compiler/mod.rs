use crate::ast::*;
use std::fmt;

pub struct Compiler {
    output: String,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            output: String::new(),
        }
    }

    pub fn compile(&mut self, program: &Program) -> Result<String, fmt::Error> {
        self.compile_program(program)?;
        Ok(self.output.clone())
    }

    fn compile_program(&mut self, program: &Program) -> fmt::Result {
        writeln!(self.output, "fn main() {{")?;
        for stmt in &program.stmts {
            self.compile_stmt(stmt)?;
        }
        writeln!(self.output, "}}")?;
        Ok(())
    }

    fn compile_stmt(&mut self, stmt: &Stmt) -> fmt::Result {
        match stmt {
            Stmt::Let(ident, expr) => {
                write!(self.output, "let {} = ", ident)?;
                self.compile_expr(expr)?;
                writeln!(self.output, ";")?;
            }
            Stmt::Expr(expr) => {
                self.compile_expr(expr)?;
                writeln!(self.output, ";")?;
            }
            Stmt::FnDef(ident, params, body) => {
                write!(self.output, "fn {}(", ident)?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(self.output, ", ")?;
                    }
                    write!(self.output, "{}", param)?;
                }
                writeln!(self.output, ") {{")?;
                self.compile_expr(body)?;
                writeln!(self.output, "}}")?;
            }
            Stmt::Return(expr) => {
                write!(self.output, "return ")?;
                self.compile_expr(expr)?;
                writeln!(self.output, ";")?;
            }
        }
        Ok(())
    }

    fn compile_expr(&mut self, expr: &Expr) -> fmt::Result {
        match expr {
            Expr::Number(value) => write!(self.output, "{}", value),
            Expr::Boolean(value) => write!(self.output, "{}", value),
            Expr::Ident(ident) => write!(self.output, "{}", ident),
            Expr::BinaryOp(left, op, right) => {
                self.compile_binary_op(left, op, right)
            }
            Expr::UnaryOp(op, expr) => {
                self.compile_unary_op(op, expr)
            }
            Expr::FnCall(ident, args) => {
                write!(self.output, "{}(", ident)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(self.output, ", ")?;
                    }
                    self.compile_expr(arg)?;
                }
                write!(self.output, ")")
            }
            Expr::If(cond, then_branch, else_branch) => {
                self.compile_if_expr(cond, then_branch, else_branch)
            }
            Expr::While(cond, body) => {
                self.compile_while_expr(cond, body)
            }
        }
    }

    fn compile_binary_op(&mut self, left: &Expr, op: &BinOp, right: &Expr) -> fmt::Result {
        self.compile_expr(left)?;
        match op {
            BinOp::Add => write!(self.output, " + ")?,
            BinOp::Sub => write!(self.output, " - ")?,
            BinOp::Mul => write!(self.output, " * ")?,
            BinOp::Div => write!(self.output, " / ")?,
            BinOp::Eq => write!(self.output, " == ")?,
            BinOp::Ne => write!(self.output, " != ")?,
            BinOp::Lt => write!(self.output, " < ")?,
            BinOp::Le => write!(self.output, " <= ")?,
            BinOp::Gt => write!(self.output, " > ")?,
            BinOp::Ge => write!(self.output, " >= ")?,
        }
        self.compile_expr(right)?;
        Ok(())
    }

    fn compile_unary_op(&mut self, op: &UnOp, expr: &Expr) -> fmt::Result {
        match op {
            UnOp::Neg => write!(self.output, "-")?,
            UnOp::Not => write!(self.output, "!")?,
        }
        self.compile_expr(expr)?;
        Ok(())
    }

    fn compile_if_expr(&mut self, cond: &Expr, then_branch: &Expr, else_branch: &Expr) -> fmt::Result {
        write!(self.output, "if ")?;
        self.compile_expr(cond)?;
        writeln!(self.output, " {{")?;
        self.compile_expr(then_branch)?;
        writeln!(self.output, "}} else {{")?;
        self.compile_expr(else_branch)?;
        writeln!(self.output, "}}")?;
        Ok(())
    }

    fn compile_while_expr(&mut self, cond: &Expr, body: &Expr) -> fmt::Result {
        write!(self.output, "while ")?;
        self.compile_expr(cond)?;
        writeln!(self.output, " {{")?;
        self.compile_expr(body)?;
        writeln!(self.output, "}}")?;
        Ok(())
    }
}