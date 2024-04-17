use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Number(f64),
    Boolean(bool),
    Ident(String),
    Keyword(Keyword),
    Operator(Operator),
    Separator(Separator),
    Eof,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    If,
    Else,
    While,
    Let,
    Fn,
    Return,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
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
    Not,
    Neg,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Separator {
    LParen,
    RParen,
    Comma,
    Semicolon,
}

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            chars: input.chars().peekable(),
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        match self.chars.next() {
            Some(c) => match c {
                '(' => Token::Separator(Separator::LParen),
                ')' => Token::Separator(Separator::RParen),
                ',' => Token::Separator(Separator::Comma),
                ';' => Token::Separator(Separator::Semicolon),
                '+' => Token::Operator(Operator::Add),
                '-' => match self.chars.peek() {
                    Some(&c) if c.is_numeric() => self.read_number(true),
                    _ => Token::Operator(Operator::Sub),
                },
                '*' => Token::Operator(Operator::Mul),
                '/' => Token::Operator(Operator::Div),
                '=' => Token::Operator(Operator::Eq),
                '!' => match self.chars.next() {
                    Some('=') => Token::Operator(Operator::Ne),
                    _ => Token::Operator(Operator::Not),
                },
                '<' => match self.chars.next() {
                    Some('=') => Token::Operator(Operator::Le),
                    _ => Token::Operator(Operator::Lt),
                },
                '>' => match self.chars.next() {
                    Some('=') => Token::Operator(Operator::Ge),
                    _ => Token::Operator(Operator::Gt),
                },
                c if c.is_alphabetic() => self.read_identifier_or_keyword(),
                c if c.is_numeric() => self.read_number(false),
                _ => panic!("Unexpected character: {}", c),
            },
            None => Token::Eof,
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(&c) = self.chars.peek() {
            if !c.is_whitespace() {
                break;
            }
            self.chars.next();
        }
    }

    fn read_identifier_or_keyword(&mut self) -> Token {
        let mut ident = String::new();
        while let Some(&c) = self.chars.peek() {
            if !c.is_alphanumeric() {
                break;
            }
            ident.push(c);
            self.chars.next();
        }

        match ident.as_str() {
            "if" => Token::Keyword(Keyword::If),
            "else" => Token::Keyword(Keyword::Else),
            "while" => Token::Keyword(Keyword::While),
            "let" => Token::Keyword(Keyword::Let),
            "fn" => Token::Keyword(Keyword::Fn),
            "return" => Token::Keyword(Keyword::Return),
            "true" => Token::Boolean(true),
            "false" => Token::Boolean(false),
            _ => Token::Ident(ident),
        }
    }

    fn read_number(&mut self, negative: bool) -> Token {
        let mut number = String::new();
        if negative {
            number.push('-');
        }
        while let Some(&c) = self.chars.peek() {
            if !c.is_numeric() && c != '.' {
                break;
            }
            number.push(c);
            self.chars.next();
        }
        Token::Number(number.parse().unwrap())
    }
}