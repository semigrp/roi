use crate::ast::*;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Number,
    Boolean,
    Function(Vec<Type>, Box<Type>),
}

#[derive(Debug)]
pub enum TypeError {
    UndefinedVariable(String),
    UndefinedFunction(String),
    InvalidOperandType(String),
    InvalidArgumentCount(String),
    ArgumentTypeMismatch(String),
    InvalidConditionType(String),
    MismatchedBranchTypes(String),
}

pub type TypeResult<T> = Result<T, TypeError>;

pub struct TypeChecker {
    env: HashMap<String, Type>,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            env: HashMap::new(),
        }
    }

    pub fn check_program(&mut self, program: &Program) -> TypeResult<()> {
        for stmt in &program.stmts {
            self.check_stmt(stmt)?;
        }
        Ok(())
    }

    fn check_stmt(&mut self, stmt: &Stmt) -> TypeResult<()> {
        match stmt {
            Stmt::Let(ident, expr) => {
                let expr_type = self.check_expr(expr)?;
                self.env.insert(ident.clone(), expr_type);
            }
            Stmt::Expr(expr) => {
                self.check_expr(expr)?;
            }
            Stmt::FnDef(ident, params, body) => {
                let param_types: Vec<Type> = params
                    .iter()
                    .map(|param| {
                        self.env
                            .get(param)
                            .cloned()
                            .unwrap_or(Type::Number)
                    })
                    .collect();
                let return_type = self.check_expr(body)?;
                let fn_type = Type::Function(param_types, Box::new(return_type));
                self.env.insert(ident.clone(), fn_type);
            }
            Stmt::Return(expr) => {
                self.check_expr(expr)?;
            }
        }
        Ok(())
    }

    fn check_expr(&mut self, expr: &Expr) -> TypeResult<Type> {
        match expr {
            Expr::Number(_) => Ok(Type::Number),
            Expr::Boolean(_) => Ok(Type::Boolean),
            Expr::Ident(ident) => {
                self.env
                    .get(ident)
                    .cloned()
                    .ok_or_else(|| TypeError::UndefinedVariable(ident.clone()))
            }
            Expr::BinaryOp(left, op, right) => self.check_binary_op(left, op, right),
            Expr::UnaryOp(op, expr) => self.check_unary_op(op, expr),
            Expr::FnCall(ident, args) => self.check_fn_call(ident, args),
            Expr::If(cond, then_branch, else_branch) => {
                self.check_if_expr(cond, then_branch, else_branch)
            }
            Expr::While(cond, body) => self.check_while_expr(cond, body),
        }
    }

    fn check_binary_op(
        &mut self,
        left: &Expr,
        op: &BinOp,
        right: &Expr,
    ) -> TypeResult<Type> {
        let left_type = self.check_expr(left)?;
        let right_type = self.check_expr(right)?;
        match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                if left_type != Type::Number || right_type != Type::Number {
                    return Err(TypeError::InvalidOperandType(
                        "Arithmetic operation requires number operands".to_string(),
                    ));
                }
                Ok(Type::Number)
            }
            BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                if left_type != right_type {
                    return Err(TypeError::InvalidOperandType(
                        "Comparison operation requires operands of the same type".to_string(),
                    ));
                }
                Ok(Type::Boolean)
            }
        }
    }

    fn check_unary_op(&mut self, op: &UnOp, expr: &Expr) -> TypeResult<Type> {
        let expr_type = self.check_expr(expr)?;
        match op {
            UnOp::Neg => {
                if expr_type != Type::Number {
                    return Err(TypeError::InvalidOperandType(
                        "Negation operation requires a number operand".to_string(),
                    ));
                }
                Ok(Type::Number)
            }
            UnOp::Not => {
                if expr_type != Type::Boolean {
                    return Err(TypeError::InvalidOperandType(
                        "Logical not operation requires a boolean operand".to_string(),
                    ));
                }
                Ok(Type::Boolean)
            }
        }
    }

    fn check_fn_call(&mut self, ident: &str, args: &[Expr]) -> TypeResult<Type> {
        let fn_type = self
            .env
            .get(ident)
            .cloned()
            .ok_or_else(|| TypeError::UndefinedFunction(ident.to_string()))?;
        match fn_type {
            Type::Function(param_types, return_type) => {
                if param_types.len() != args.len() {
                    return Err(TypeError::InvalidArgumentCount(format!(
                        "Function {} expects {} arguments, but {} were given",
                        ident,
                        param_types.len(),
                        args.len()
                    )));
                }
                for (param_type, arg) in param_types.iter().zip(args.iter()) {
                    let arg_type = self.check_expr(arg)?;
                    if *param_type != arg_type {
                        return Err(TypeError::ArgumentTypeMismatch(format!(
                            "Argument type mismatch in function call to {}",
                            ident
                        )));
                    }
                }
                Ok(*return_type)
            }
            _ => Err(TypeError::UndefinedFunction(format!(
                "{} is not a function",
                ident
            ))),
        }
    }

    fn check_if_expr(
        &mut self,
        cond: &Expr,
        then_branch: &Expr,
        else_branch: &Expr,
    ) -> TypeResult<Type> {
        let cond_type = self.check_expr(cond)?;
        if cond_type != Type::Boolean {
            return Err(TypeError::InvalidConditionType(
                "If condition must be a boolean expression".to_string(),
            ));
        }
        let then_type = self.check_expr(then_branch)?;
        let else_type = self.check_expr(else_branch)?;
        if then_type != else_type {
            return Err(TypeError::MismatchedBranchTypes(
                "If branches must have the same type".to_string(),
            ));
        }
        Ok(then_type)
    }

    fn check_while_expr(&mut self, cond: &Expr, body: &Expr) -> TypeResult<Type> {
        let cond_type = self.check_expr(cond)?;
        if cond_type != Type::Boolean {
            return Err(TypeError::InvalidConditionType(
                "While condition must be a boolean expression".to_string(),
            ));
        }
        self.check_expr(body)?;
        Ok(Type::Boolean)
    }
}