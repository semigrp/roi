use crate::ast::*;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Number,
    Boolean,
    Function(Vec<Type>, Box<Type>),
}

pub struct TypeChecker {
    env: HashMap<String, Type>,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            env: HashMap::new(),
        }
    }

    pub fn check_program(&mut self, program: &Program) {
        for stmt in &program.stmts {
            self.check_stmt(stmt);
        }
    }

    fn check_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let(ident, expr) => {
                let expr_type = self.check_expr(expr);
                self.env.insert(ident.clone(), expr_type);
            }
            Stmt::Expr(expr) => {
                self.check_expr(expr);
            }
            Stmt::FnDef(ident, params, body) => {
                let param_types: Vec<Type> = params
                    .iter()
                    .map(|param| self.env.get(param).unwrap_or(&Type::Number).clone())
                    .collect();
                let return_type = self.check_expr(body);
                let fn_type = Type::Function(param_types, Box::new(return_type));
                self.env.insert(ident.clone(), fn_type);
            }
            Stmt::Return(expr) => {
                self.check_expr(expr);
            }
        }
    }

    fn check_expr(&mut self, expr: &Expr) -> Type {
        match expr {
            Expr::Number(_) => Type::Number,
            Expr::Boolean(_) => Type::Boolean,
            Expr::Ident(ident) => self.env.get(ident).unwrap_or(&Type::Number).clone(),
            Expr::BinaryOp(left, op, right) => self.check_binary_op(left, op, right),
            Expr::UnaryOp(op, expr) => self.check_unary_op(op, expr),
            Expr::FnCall(ident, args) => self.check_fn_call(ident, args),
            Expr::If(cond, then_branch, else_branch) => self.check_if_expr(cond, then_branch, else_branch),
            Expr::While(cond, body) => self.check_while_expr(cond, body),
        }
    }

    fn check_binary_op(&mut self, left: &Expr, op: &BinOp, right: &Expr) -> Type {
        let left_type = self.check_expr(left);
        let right_type = self.check_expr(right);
        match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                if left_type != Type::Number || right_type != Type::Number {
                    panic!("Invalid operand types for arithmetic operation");
                }
                Type::Number
            }
            BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                if left_type != right_type {
                    panic!("Invalid operand types for comparison operation");
                }
                Type::Boolean
            }
        }
    }

    fn check_unary_op(&mut self, op: &UnOp, expr: &Expr) -> Type {
        let expr_type = self.check_expr(expr);
        match op {
            UnOp::Neg => {
                if expr_type != Type::Number {
                    panic!("Invalid operand type for negation operation");
                }
                Type::Number
            }
            UnOp::Not => {
                if expr_type != Type::Boolean {
                    panic!("Invalid operand type for logical not operation");
                }
                Type::Boolean
            }
        }
    }

    fn check_fn_call(&mut self, ident: &str, args: &[Expr]) -> Type {
        let fn_type = self.env.get(ident).unwrap_or_else(|| {
            panic!("Undefined function: {}", ident);
        });
        match fn_type {
            Type::Function(param_types, return_type) => {
                if param_types.len() != args.len() {
                    panic!("Incorrect number of arguments for function call");
                }
                for (param_type, arg) in param_types.iter().zip(args.iter()) {
                    let arg_type = self.check_expr(arg);
                    if *param_type != arg_type {
                        panic!("Argument type mismatch in function call");
                    }
                }
                *return_type.clone()
            }
            _ => panic!("Not a function: {}", ident),
        }
    }

    fn check_if_expr(&mut self, cond: &Expr, then_branch: &Expr, else_branch: &Expr) -> Type {
        let cond_type = self.check_expr(cond);
        if cond_type != Type::Boolean {
            panic!("Invalid condition type for if expression");
        }
        let then_type = self.check_expr(then_branch);
        let else_type = self.check_expr(else_branch);
        if then_type != else_type {
            panic!("Mismatched types in if expression branches");
        }
        then_type
    }

    fn check_while_expr(&mut self, cond: &Expr, body: &Expr) -> Type {
        let cond_type = self.check_expr(cond);
        if cond_type != Type::Boolean {
            panic!("Invalid condition type for while expression");
        }
        self.check_expr(body);
        Type::Boolean
    }
}