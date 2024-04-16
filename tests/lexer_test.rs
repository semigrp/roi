#[cfg(test)]
mod tests {
    use roi::lexer::Lexer;
    use roi::token::{Token, TokenType};

    #[test]
    fn test_next_token() {
        let input = "=+(){},;";
        let expected_tokens = vec![
            Token { token_type: TokenType::Assign, literal: "=".to_string() },
            Token { token_type: TokenType::Plus, literal: "+".to_string() },
            Token { token_type: TokenType::LParen, literal: "(".to_string() },
            Token { token_type: TokenType::RParen, literal: ")".to_string() },
            Token { token_type: TokenType::LBrace, literal: "{".to_string() },
            Token { token_type: TokenType::RBrace, literal: "}".to_string() },
            Token { token_type: TokenType::Comma, literal: ",".to_string() },
            Token { token_type: TokenType::Semicolon, literal: ";".to_string() },
        ];

        let mut lexer = Lexer::new(input.to_string());

        for expected in expected_tokens {
            let token = lexer.next_token();
            assert_eq!(token.token_type, expected.token_type);
            assert_eq!(token.literal, expected.literal);
        }
    }
}
