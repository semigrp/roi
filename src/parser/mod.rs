use crate::ast::*;
use crate::lexer::{Lexer, Token};
use std::borrow::BorrowMut;
use std::cell::RefCell;

pub struct Parser<'a> {
    lexer: RefCell<Lexer<'a>>,
    current_token: RefCell<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let current_token = lexer.next_token();
        Parser {
            lexer: RefCell::new(lexer),
            current_token: RefCell::new(current_token),
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut stmts = Vec::new();
        while !self.is_end() {
            stmts.push(self.parse_stmt());
        }
        Program { stmts }
    }

    fn parse_stmt(&mut self) -> Stmt {
        match self.current_token {
            Token::Keyword(Keyword::Let) => self.parse_let_stmt(),
            Token::Keyword(Keyword::Fn) => self.parse_fn_def(),
            Token::Keyword(Keyword::Return) => self.parse_return_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_let_stmt(&mut self) -> Stmt {
        self.expect_keyword(Keyword::Let);
        let ident = self.parse_ident();
        self.expect_operator(Operator::Eq);
        let expr = self.parse_expr();
        Stmt::Let(ident, expr)
    }

    fn parse_fn_def(&mut self) -> Stmt {
        self.expect_keyword(Keyword::Fn);
        let ident = self.parse_ident();
        let mut params = Vec::new();
        while !self.is_separator(Separator::Semicolon) {
            params.push(self.parse_ident());
        }
        self.expect_separator(Separator::Semicolon);
        let body = self.parse_expr();
        Stmt::FnDef(ident, params, Box::new(body))
    }

    fn parse_return_stmt(&mut self) -> Stmt {
        self.expect_keyword(Keyword::Return);
        let expr = self.parse_expr();
        Stmt::Return(expr)
    }

    fn parse_expr_stmt(&mut self) -> Stmt {
        let expr = self.parse_expr();
        Stmt::Expr(expr)
    }

    fn parse_expr(&mut self) -> Expr {
        let mut stack = Vec::new();
        while !self.is_end() && !self.is_separator(Separator::Semicolon) {
            match &self.current_token {
                Token::Number(value) => {
                    stack.push(Expr::Number(*value));
                    self.advance();
                }
                Token::Boolean(value) => {
                    stack.push(Expr::Boolean(*value));
                    self.advance();
                }
                Token::Ident(ident) => {
                    stack.push(Expr::Ident(ident.clone()));
                    self.advance();
                }
                Token::Operator(operator) => {
                    let op = self.parse_operator();
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();
                    stack.push(Expr::BinaryOp(Box::new(left), op, Box::new(right)));
                }
                Token::Keyword(Keyword::If) => {
                    self.advance();
                    let cond = stack.pop().unwrap();
                    let then_branch = stack.pop().unwrap();
                    let else_branch = stack.pop().unwrap();
                    stack.push(Expr::If(
                        Box::new(cond),
                        Box::new(then_branch),
                        Box::new(else_branch),
                    ));
                }
                Token::Keyword(Keyword::While) => {
                    self.advance();
                    let cond = stack.pop().unwrap();
                    let body = stack.pop().unwrap();
                    stack.push(Expr::While(Box::new(cond), Box::new(body)));
                }
                _ => panic!("Unexpected token: {:?}", self.current_token),
            }
        }
        assert_eq!(stack.len(), 1);
        stack.pop().unwrap()
    }

    fn parse_ident(&mut self) -> String {
        match &self.current_token {
            Token::Ident(ident) => {
                let ident = ident.clone();
                self.advance();
                ident
            }
            _ => panic!("Expected identifier, found: {:?}", self.current_token),
        }
    }

    fn parse_operator(&mut self) -> BinOp {
        match self.current_token {
            Token::Operator(Operator::Add) => {
                self.advance();
                BinOp::Add
            }
            Token::Operator(Operator::Sub) => {
                self.advance();
                BinOp::Sub
            }
            Token::Operator(Operator::Mul) => {
                self.advance();
                BinOp::Mul
            }
            Token::Operator(Operator::Div) => {
                self.advance();
                BinOp::Div
            }
            Token::Operator(Operator::Eq) => {
                self.advance();
                BinOp::Eq
            }
            Token::Operator(Operator::Ne) => {
                self.advance();
                BinOp::Ne
            }
            Token::Operator(Operator::Lt) => {
                self.advance();
                BinOp::Lt
            }
            Token::Operator(Operator::Le) => {
                self.advance();
                BinOp::Le
            }
            Token::Operator(Operator::Gt) => {
                self.advance();
                BinOp::Gt
            }
            Token::Operator(Operator::Ge) => {
                self.advance();
                BinOp::Ge
            }
            _ => panic!("Expected operator, found: {:?}", self.current_token),
        }
    }

    fn advance(&self) {
        *self.current_token.borrow_mut() = self.lexer.borrow_mut().next_token();
    }

    fn is_end(&self) -> bool {
        matches!(self.current_token, Token::Eof)
    }

    fn is_keyword(&self, keyword: Keyword) -> bool {
        matches!(self.current_token, Token::Keyword(kw) if kw == keyword)
    }

    fn is_operator(&self, operator: Operator) -> bool {
        matches!(self.current_token, Token::Operator(op) if op == operator)
    }

    fn is_separator(&self, separator: Separator) -> bool {
        matches!(self.current_token, Token::Separator(sep) if sep == separator)
    }

    fn expect_keyword(&mut self, keyword: Keyword) {
        if !self.is_keyword(keyword) {
            panic!("Expected keyword: {:?}, found: {:?}", keyword, self.current_token);
        }
        self.advance();
    }

    fn expect_operator(&mut self, operator: Operator) {
        if !self.is_operator(operator) {
            panic!("Expected operator: {:?}, found: {:?}", operator, self.current_token);
        }
        self.advance();
    }

    fn expect_separator(&mut self, separator: Separator) {
        if !self.is_separator(separator) {
            panic!("Expected separator: {:?}, found: {:?}", separator, self.current_token);
        }
        self.advance();
    }
}