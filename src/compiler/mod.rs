use crate::ast::*;
use crate::typechecker::{TypeChecker, TypeError};
use std::collections::HashMap;
use std::fmt;

#[derive(Debug)]
pub enum CompileError {
    TypeCheckError(TypeError),
    CodeGenError(fmt::Error),
    UndefinedVariable(String),
    UndefinedFunction(String),
    FunctionRedefinition(String),
    InconsistentArgCount {
        ident: String,
        expected: usize,
        actual: usize,
    },
    StackUnderflow,
    InvalidOperator(String),
    InvalidUnaryOperator(String),
    InvalidExpression,
    InvalidStatement,
    InvalidFunctionDefinition,
    InvalidReturnStatement,
    InvalidLetStatement,
    InvalidIfExpression,
    InvalidWhileExpression,
    InvalidFunctionCall,
    InvalidArgument,
    InvalidNumber(String),
    InvalidBoolean(String),
    InvalidIdentifier(String),
    UnknownError(String),
}

impl From<TypeError> for CompileError {
    fn from(error: TypeError) -> Self {
        CompileError::TypeCheckError(error)
    }
}

impl From<fmt::Error> for CompileError {
    fn from(error: fmt::Error) -> Self {
        CompileError::CodeGenError(error)
    }
}

pub struct Compiler {
    output: String,
    symbol_table: HashMap<String, FunctionDef>,
}

struct FunctionDef {
    arity: usize,
    name: String,
    param_names: Vec<String>,
    return_type: Option<Type>,
    body: Expr,
    signature: String,
    is_recursive: bool,
    is_variadic: bool,
    is_higher_order: bool,
    is_closure: bool,
    capture_env: Option<HashMap<String, Type>>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            output: String::new(),
            symbol_table: HashMap::new(),
        }
    }

    pub fn compile(&mut self, program: &Program) -> Result<String, CompileError> {
        self.build_symbol_table(program)?;
        let mut type_checker = TypeChecker::new();
        type_checker.check_program(program)?;
        self.compile_program(program)?;
        Ok(self.output.clone())
    }

    fn build_symbol_table(&mut self, program: &Program) -> Result<(), CompileError> {
        for stmt in &program.stmts {
            if let Stmt::FnDef(ident, params, body) = stmt {
                if self.symbol_table.contains_key(ident) {
                    return Err(CompileError::FunctionRedefinition(ident.clone()));
                }

                let param_names: Vec<String> = params.iter().map(|param| param.clone()).collect();

                let mut param_types = Vec::new();
                for param in params {
                    let param_type = self.infer_type(param)?;
                    param_types.push(param_type);
                }

                let return_type = self.infer_type(body)?;

                let signature = format!(
                    "{}({}) -> {}",
                    ident,
                    param_types
                        .iter()
                        .zip(param_names.iter())
                        .map(|(ty, name)| format!("{}: {}", name, ty))
                        .collect::<Vec<String>>()
                        .join(", "),
                    return_type
                );

                let is_recursive = self.is_recursive(ident, body);

                let is_variadic = params.last().map_or(false, |param| param.starts_with("..."));

                let is_higher_order =
                    param_types.iter().any(|ty| matches!(ty, Type::Function(..))) || matches!(return_type, Type::Function(..));

                let (is_closure, capture_env) = self.analyze_closure(ident, params, body)?;

                let fn_def = FunctionDef {
                    arity: params.len(),
                    name: ident.clone(),
                    param_names,
                    return_type,
                    body: body.clone(),
                    signature,
                    is_recursive,
                    is_variadic,
                    is_higher_order,
                    is_closure,
                    capture_env,
                };

                self.symbol_table.insert(ident.clone(), fn_def);
            }
        }

        Ok(())
    }

    fn infer_type(&self, expr: &Expr) -> Result<Type, CompileError> {
        match expr {
            Expr::Number(_) => Ok(Type::Number),
            Expr::Boolean(_) => Ok(Type::Boolean),
            Expr::Ident(ident) => {
                if let Some(fn_def) = self.symbol_table.get(ident) {
                    Ok(Type::Function(
                        fn_def.param_types.clone(),
                        Box::new(fn_def.return_type.clone().unwrap()),
                    ))
                } else {
                    Err(CompileError::UndefinedVariable(ident.clone()))
                }
            }
            Expr::BinaryOp(left, _, right) => {
                let left_type = self.infer_type(left)?;
                let right_type = self.infer_type(right)?;
                if left_type == right_type {
                    Ok(left_type)
                } else {
                    Err(CompileError::TypeError(format!(
                        "Mismatched types in binary operation: {} and {}",
                        left_type, right_type
                    )))
                }
            }
            Expr::UnaryOp(_, expr) => self.infer_type(expr),
            Expr::FnCall(ident, args) => {
                if let Some(fn_def) = self.symbol_table.get(ident) {
                    if fn_def.param_types.len() != args.len() {
                        return Err(CompileError::InconsistentArgCount {
                            ident: ident.clone(),
                            expected: fn_def.param_types.len(),
                            actual: args.len(),
                        });
                    }
                    for (param_type, arg) in fn_def.param_types.iter().zip(args.iter()) {
                        let arg_type = self.infer_type(arg)?;
                        if param_type != &arg_type {
                            return Err(CompileError::TypeError(format!(
                                "Mismatched types in function call: expected {}, found {}",
                                param_type, arg_type
                            )));
                        }
                    }
                    Ok(fn_def.return_type.clone().unwrap())
                } else {
                    Err(CompileError::UndefinedFunction(ident.clone()))
                }
            }
            Expr::If(_, then_branch, else_branch) => {
                let then_type = self.infer_type(then_branch)?;
                let else_type = self.infer_type(else_branch)?;
                if then_type == else_type {
                    Ok(then_type)
                } else {
                    Err(CompileError::TypeError(format!(
                        "Mismatched types in if-else expression: {} and {}",
                        then_type, else_type
                    )))
                }
            }
            Expr::While(_, body) => self.infer_type(body),
        }
    }

    fn is_recursive(&self, ident: &str, body: &Expr) -> bool {
        match body {
            Expr::Ident(id) => ident == id,
            Expr::BinaryOp(left, _, right) => self.is_recursive(ident, left) || self.is_recursive(ident, right),
            Expr::UnaryOp(_, expr) => self.is_recursive(ident, expr),
            Expr::FnCall(id, _) => ident == id,
            Expr::If(_, then_branch, else_branch) => {
                self.is_recursive(ident, then_branch) || self.is_recursive(ident, else_branch)
            }
            Expr::While(_, body) => self.is_recursive(ident, body),
            _ => false,
        }
    }

    fn analyze_closure(
        &self,
        ident: &str,
        params: &[String],
        body: &Expr,
    ) -> Result<(bool, Option<HashMap<String, Type>>), CompileError> {
        let mut is_closure = false;
        let mut capture_env = HashMap::new();

        self.collect_free_vars(body, &mut capture_env)?;

        if !capture_env.is_empty() {
            is_closure = true;
        }

        for (var, _) in &mut capture_env {
            let var_type = self.infer_type(&Expr::Ident(var.clone()))?;
            *capture_env.get_mut(var).unwrap() = var_type;
        }

        Ok((is_closure, if is_closure { Some(capture_env) } else { None }))
    }

    fn collect_free_vars(
        &self,
        expr: &Expr,
        free_vars: &mut HashMap<String, Type>,
    ) -> Result<(), CompileError> {
        match expr {
            Expr::Ident(ident) => {
                if !self.symbol_table.contains_key(ident) {
                    free_vars.insert(ident.clone(), Type::Unknown);
                }
                Ok(())
            }
            Expr::BinaryOp(left, _, right) => {
                self.collect_free_vars(left, free_vars)?;
                self.collect_free_vars(right, free_vars)
            }
            Expr::UnaryOp(_, expr) => self.collect_free_vars(expr, free_vars),
            Expr::FnCall(_, args) => {
                for arg in args {
                    self.collect_free_vars(arg, free_vars)?;
                }
                Ok(())
            }
            Expr::If(cond, then_branch, else_branch) => {
                self.collect_free_vars(cond, free_vars)?;
                self.collect_free_vars(then_branch, free_vars)?;
                self.collect_free_vars(else_branch, free_vars)
            }
            Expr::While(cond, body) => {
                self.collect_free_vars(cond, free_vars)?;
                self.collect_free_vars(body, free_vars)
            }
            _ => Ok(()),
        }
    }

    fn compile_program(&mut self, program: &Program) -> Result<(), CompileError> {
        writeln!(self.output, "fn main() {{")?;
        for stmt in &program.stmts {
            self.compile_stmt(stmt)?;
        }
        writeln!(self.output, "}}")?;
        Ok(())
    }

    fn compile_stmt(&mut self, stmt: &Stmt) -> Result<(), CompileError> {
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

    fn compile_expr(&mut self, expr: &Expr) -> Result<(), CompileError> {
        let mut stack: Vec<String> = Vec::new();
        for token in expr {
            match token {
                Expr::Number(value) => stack.push(format!("{}", value)),
                Expr::Boolean(value) => stack.push(format!("{}", value)),
                Expr::Ident(ident) => {
                    if let Some(_) = self.symbol_table.get(ident) {
                        stack.push(format!("{}", ident));
                    } else {
                        return Err(CompileError::UndefinedVariable(ident.clone()));
                    }
                }
                Expr::Operator(op) => {
                    let right = stack.pop().ok_or(CompileError::StackUnderflow)?;
                    let left = stack.pop().ok_or(CompileError::StackUnderflow)?;
                    let code = self.compile_binary_op(&left, op, &right)?;
                    stack.push(code);
                }
                Expr::UnaryOp(op, _) => {
                    let expr_code = stack.pop().ok_or(CompileError::StackUnderflow)?;
                    let code = self.compile_unary_op(op, &expr_code)?;
                    stack.push(code);
                }
                Expr::FnCall(ident, _) => {
                    let args_count = self.symbol_table.get(ident).map(|def| def.arity).unwrap_or(0);
                    let mut args: Vec<String> = Vec::new();
                    for _ in 0..args_count {
                        let arg = stack.pop().ok_or(CompileError::StackUnderflow)?;
                        args.push(arg);
                    }
                    args.reverse();
                    let code = self.compile_fn_call(ident, &args)?;
                    stack.push(code);
                }
                Expr::If(_, _, _) => {
                    let else_branch = stack.pop().ok_or(CompileError::StackUnderflow)?;
                    let then_branch = stack.pop().ok_or(CompileError::StackUnderflow)?;
                    let cond = stack.pop().ok_or(CompileError::StackUnderflow)?;
                    let code = self.compile_if_expr(&cond, &then_branch, &else_branch)?;
                    stack.push(code);
                }
                Expr::While(_, _) => {
                    let body = stack.pop().ok_or(CompileError::StackUnderflow)?;
                    let cond = stack.pop().ok_or(CompileError::StackUnderflow)?;
                    let code = self.compile_while_expr(&cond, &body)?;
                    stack.push(code);
                }
            }
        }
        if stack.len() != 1 {
            return Err(CompileError::StackUnderflow);
        }
        write!(self.output, "{}", stack.pop().ok_or(CompileError::StackUnderflow)?)?;
        Ok(())
    }

    fn compile_binary_op(
        &mut self,
        left: &str,
        op: &BinOp,
        right: &str,
    ) -> Result<String, CompileError> {
        let op_str = match op {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Eq => "==",
            BinOp::Ne => "!=",
            BinOp::Lt => "<",
            BinOp::Le => "<=",
            BinOp::Gt => ">",
            BinOp::Ge => ">=",
        };
        Ok(format!("({} {} {})", left, op_str, right))
    }

    fn compile_unary_op(&mut self, op: &UnOp, expr: &str) -> Result<String, CompileError> {
        let op_str = match op {
            UnOp::Neg => "-",
            UnOp::Not => "!",
        };
        Ok(format!("({}{})", op_str, expr))
    }

    fn compile_fn_call(
        &mut self,
        ident: &str,
        args: &[String],
    ) -> Result<String, CompileError> {
        let fn_def = self.symbol_table.get(ident).ok_or_else(|| {
            CompileError::UndefinedFunction(ident.to_string())
        })?;

        if fn_def.arity != args.len() {
            return Err(CompileError::InconsistentArgCount {
                ident: ident.to_string(),
                expected: fn_def.arity,
                actual: args.len(),
            });
        }

        let arg_str = args.join(", ");
        Ok(format!("{}({})", ident, arg_str))
    }

    fn compile_if_expr(
        &mut self,
        cond: &str,
        then_branch: &str,
        else_branch: &str,
    ) -> Result<String, CompileError> {
        Ok(format!("if {} {{ {} }} else {{ {} }}", cond, then_branch, else_branch))
    }

    fn compile_while_expr(
        &mut self,
        cond: &str,
        body: &str,
    ) -> Result<String, CompileError> {
        Ok(format!("while {} {{ {} }}", cond, body))
    }
}
