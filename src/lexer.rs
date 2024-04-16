use crate::token::{Token, TokenType};

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: '\0',
        };
        lexer.read_char();
        lexer
    }

    pub fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input.chars().nth(self.read_position).unwrap_or('\0');
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Token {
        let token = match self.ch {
            '=' => Token { token_type: TokenType::Assign, literal: self.ch.to_string() },
            ';' => Token { token_type: TokenType::Semicolon, literal: self.ch.to_string() },
            '(' => Token { token_type: TokenType::LParen, literal: self.ch.to_string() },
            ')' => Token { token_type: TokenType::RParen, literal: self.ch.to_string() },
            ',' => Token { token_type: TokenType::Comma, literal: self.ch.to_string() },
            '+' => Token { token_type: TokenType::Plus, literal: self.ch.to_string() },
            '{' => Token { token_type: TokenType::LBrace, literal: self.ch.to_string() },
            '}' => Token { token_type: TokenType::RBrace, literal: self.ch.to_string() },
            _ => Token { token_type: TokenType::Illegal, literal: "".to_string() },
        };

        self.read_char();
        token
    }
}
