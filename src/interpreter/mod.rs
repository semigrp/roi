use crate::ast::*;
use std::collections::HashMap;

pub struct Interpreter {
    env: HashMap<String, Value>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            env: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, program: &Program) -> Result<Value, String> {
        self.interpret_program(program)
    }

    fn interpret_program(&mut self, program: &Program) -> Result<Value, String> {
        let mut last_value = Value::Null;
        for stmt in &program.stmts {
            last_value = self.interpret_stmt(stmt)?;
        }
        Ok(last_value)
    }

    fn interpret_stmt(&mut self, stmt: &Stmt) -> Result<Value, String> {
        match stmt {
            Stmt::Let(ident, expr) => {
                let value = self.interpret_expr(expr)?;
                self.env.insert(ident.clone(), value);
                Ok(Value::Null)
            }
            Stmt::Expr(expr) => self.interpret_expr(expr),
            Stmt::FnDef(ident, params, body) => {
                let closure = Closure {
                    params: params.clone(),
                    body: body.clone(),
                    env: self.env.clone(),
                };
                self.env.insert(ident.clone(), Value::Function(closure));
                Ok(Value::Null)
            }
            Stmt::Return(expr) => {
                let value = self.interpret_expr(expr)?;
                Err(format!("return {}", value))
            }
        }
    }

    fn interpret_expr(&mut self, expr: &Expr) -> Result<Value, String> {
        match expr {
            Expr::Number(value) => Ok(Value::Number(*value)),
            Expr::Boolean(value) => Ok(Value::Boolean(*value)),
            Expr::Ident(ident) => match self.env.get(ident) {
                Some(value) => Ok(value.clone()),
                None => Err(format!("Undefined variable: {}", ident)),
            },
            Expr::BinaryOp(left, op, right) => {
                let left_value = self.interpret_expr(left)?;
                let right_value = self.interpret_expr(right)?;
                self.interpret_binary_op(&left_value, op, &right_value)
            }
            Expr::UnaryOp(op, expr) => {
                let value = self.interpret_expr(expr)?;
                self.interpret_unary_op(op, &value)
            }
            Expr::FnCall(ident, args) => {
                let function = match self.env.get(ident) {
                    Some(Value::Function(closure)) => closure.clone(),
                    _ => return Err(format!("Not a function: {}", ident)),
                };
                let mut arg_values = Vec::new();
                for arg in args {
                    let value = self.interpret_expr(arg)?;
                    arg_values.push(value);
                }
                self.interpret_fn_call(function, &arg_values)
            }
            Expr::If(cond, then_branch, else_branch) => {
                let cond_value = self.interpret_expr(cond)?;
                self.interpret_if_expr(&cond_value, then_branch, else_branch)
            }
            Expr::While(cond, body) => self.interpret_while_expr(cond, body),
        }
    }

    fn interpret_binary_op(
        &self,
        left: &Value,
        op: &BinOp,
        right: &Value,
    ) -> Result<Value, String> {
        match (left, op, right) {
            (Value::Number(left), BinOp::Add, Value::Number(right)) => {
                Ok(Value::Number(left + right))
            }
            (Value::Number(left), BinOp::Sub, Value::Number(right)) => {
                Ok(Value::Number(left - right))
            }
            (Value::Number(left), BinOp::Mul, Value::Number(right)) => {
                Ok(Value::Number(left * right))
            }
            (Value::Number(left), BinOp::Div, Value::Number(right)) => {
                if *right == 0.0 {
                    Err("Division by zero".to_string())
                } else {
                    Ok(Value::Number(left / right))
                }
            }
            (Value::Number(left), BinOp::Eq, Value::Number(right)) => {
                Ok(Value::Boolean(left == right))
            }
            (Value::Number(left), BinOp::Ne, Value::Number(right)) => {
                Ok(Value::Boolean(left != right))
            }
            (Value::Number(left), BinOp::Lt, Value::Number(right)) => {
                Ok(Value::Boolean(left < right))
            }
            (Value::Number(left), BinOp::Le, Value::Number(right)) => {
                Ok(Value::Boolean(left <= right))
            }
            (Value::Number(left), BinOp::Gt, Value::Number(right)) => {
                Ok(Value::Boolean(left > right))
            }
            (Value::Number(left), BinOp::Ge, Value::Number(right)) => {
                Ok(Value::Boolean(left >= right))
            }
            _ => Err(format!("Invalid operands for binary operation: {:?} {:?} {:?}", left, op, right)),
        }
    }

    fn interpret_unary_op(&self, op: &UnOp, value: &Value) -> Result<Value, String> {
        match (op, value) {
            (UnOp::Neg, Value::Number(value)) => Ok(Value::Number(-value)),
            (UnOp::Not, Value::Boolean(value)) => Ok(Value::Boolean(!value)),
            _ => Err(format!("Invalid operand for unary operation: {:?} {:?}", op, value)),
        }
    }

    fn interpret_fn_call(
        &mut self,
        closure: Closure,
        args: &[Value],
    ) -> Result<Value, String> {
        if closure.params.len() != args.len() {
            return Err(format!(
                "Incorrect number of arguments. Expected {}, got {}",
                closure.params.len(),
                args.len()
            ));
        }

        let mut new_env = closure.env.clone();
        for (param, arg) in closure.params.iter().zip(args.iter()) {
            new_env.insert(param.clone(), arg.clone());
        }

        let prev_env = std::mem::replace(&mut self.env, new_env);
        let result = self.interpret_expr(&closure.body);
        self.env = prev_env;

        match result {
            Ok(value) => Ok(value),
            Err(msg) => {
                if msg.starts_with("return ") {
                    Ok(Value::from_str(&msg[7..]).unwrap())
                } else {
                    Err(msg)
                }
            }
        }
    }

    fn interpret_if_expr(
        &mut self,
        cond_value: &Value,
        then_branch: &Expr,
        else_branch: &Expr,
    ) -> Result<Value, String> {
        match cond_value {
            Value::Boolean(true) => self.interpret_expr(then_branch),
            Value::Boolean(false) => self.interpret_expr(else_branch),
            _ => Err(format!("Invalid condition for if expression: {:?}", cond_value)),
        }
    }

    fn interpret_while_expr(&mut self, cond: &Expr, body: &Expr) -> Result<Value, String> {
        while let Value::Boolean(true) = self.interpret_expr(cond)? {
            self.interpret_expr(body)?;
        }
        Ok(Value::Null)
    }
}

#[derive(Debug, Clone)]
enum Value {
    Number(f64),
    Boolean(bool),
    Function(Closure),
    Null,
}

impl Value {
    fn from_str(s: &str) -> Option<Value> {
        if let Ok(num) = s.parse::<f64>() {
            Some(Value::Number(num))
        } else if let Ok(bool) = s.parse::<bool>() {
            Some(Value::Boolean(bool))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
struct Closure {
    params: Vec<String>,
    body: Box<Expr>,
    env: HashMap<String, Value>,
}