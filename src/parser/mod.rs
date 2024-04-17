use crate::ast::*;
use crate::lexer::{Lexer, Token};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let current_token = lexer.next_token();
        Parser {
            lexer,
            current_token,
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
        self.expect_separator(Separator::LParen);
        let mut params = Vec::new();
        while !self.is_separator(Separator::RParen) {
            params.push(self.parse_ident());
            if !self.is_separator(Separator::RParen) {
                self.expect_separator(Separator::Comma);
            }
        }
        self.expect_separator(Separator::RParen);
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
        self.parse_equality()
    }

    fn parse_equality(&mut self) -> Expr {
        let mut expr = self.parse_comparison();
        while self.is_operator(Operator::Eq) || self.is_operator(Operator::Ne) {
            let op = self.parse_operator();
            let right = self.parse_comparison();
            expr = Expr::BinaryOp(Box::new(expr), op, Box::new(right));
        }
        expr
    }

    fn parse_comparison(&mut self) -> Expr {
        let mut expr = self.parse_term();
        while self.is_operator(Operator::Lt)
            || self.is_operator(Operator::Le)
            || self.is_operator(Operator::Gt)
            || self.is_operator(Operator::Ge)
        {
            let op = self.parse_operator();
            let right = self.parse_term();
            expr = Expr::BinaryOp(Box::new(expr), op, Box::new(right));
        }
        expr
    }

    fn parse_term(&mut self) -> Expr {
        let mut expr = self.parse_factor();
        while self.is_operator(Operator::Add) || self.is_operator(Operator::Sub) {
            let op = self.parse_operator();
            let right = self.parse_factor();
            expr = Expr::BinaryOp(Box::new(expr), op, Box::new(right));
        }
        expr
    }

    fn parse_factor(&mut self) -> Expr {
        let mut expr = self.parse_unary();
        while self.is_operator(Operator::Mul) || self.is_operator(Operator::Div) {
            let op = self.parse_operator();
            let right = self.parse_unary();
            expr = Expr::BinaryOp(Box::new(expr), op, Box::new(right));
        }
        expr
    }

    fn parse_unary(&mut self) -> Expr {
        if self.is_operator(Operator::Sub) || self.is_operator(Operator::Not) {
            let op = self.parse_operator();
            let expr = self.parse_unary();
            Expr::UnaryOp(op, Box::new(expr))
        } else {
            self.parse_primary()
        }
    }

    fn parse_primary(&mut self) -> Expr {
        match &self.current_token {
            Token::Number(value) => {
                let expr = Expr::Number(*value);
                self.advance();
                expr
            }
            Token::Boolean(value) => {
                let expr = Expr::Boolean(*value);
                self.advance();
                expr
            }
            Token::Ident(ident) => {
                let expr = if self.is_separator(Separator::LParen) {
                    self.parse_fn_call(ident.clone())
                } else {
                    Expr::Ident(ident.clone())
                };
                self.advance();
                expr
            }
            Token::Separator(Separator::LParen) => {
                self.advance();
                let expr = self.parse_expr();
                self.expect_separator(Separator::RParen);
                expr
            }
            Token::Keyword(Keyword::If) => self.parse_if_expr(),
            Token::Keyword(Keyword::While) => self.parse_while_expr(),
            _ => panic!("Unexpected token: {:?}", self.current_token),
        }
    }

    fn parse_fn_call(&mut self, ident: String) -> Expr {
        self.expect_separator(Separator::LParen);
        let mut args = Vec::new();
        while !self.is_separator(Separator::RParen) {
            args.push(self.parse_expr());
            if !self.is_separator(Separator::RParen) {
                self.expect_separator(Separator::Comma);
            }
        }
        self.expect_separator(Separator::RParen);
        Expr::FnCall(ident, args)
    }

    fn parse_if_expr(&mut self) -> Expr {
        self.expect_keyword(Keyword::If);
        let cond = self.parse_expr();
        let then_branch = self.parse_expr();
        let else_branch = if self.is_keyword(Keyword::Else) {
            self.advance();
            self.parse_expr()
        } else {
            Expr::Boolean(false)
        };
        Expr::If(Box::new(cond), Box::new(then_branch), Box::new(else_branch))
    }

    fn parse_while_expr(&mut self) -> Expr {
        self.expect_keyword(Keyword::While);
        let cond = self.parse_expr();
        let body = self.parse_expr();
        Expr::While(Box::new(cond), Box::new(body))
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

    fn advance(&mut self) {
        self.current_token = self.lexer.next_token();
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