use crate::{
    scanner::Scanner,
    token::{self, RawTokenKind, Token, TokenKind},
};

pub struct Lexer<'a> {
    source: &'a str,
    scanner: Scanner<'a>,
    offset: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            scanner: Scanner::new(source),
            offset: 0,
        }
    }

    pub fn next_token(&mut self) -> Token {
        let raw_token = self.scanner.next_raw();

        let literal = &self.source[self.offset..self.offset + raw_token.len];

        match raw_token.kind {
            RawTokenKind::Eof => {
                self.offset += raw_token.len;
                Token::new(TokenKind::Eof, raw_token.len)
            }
            RawTokenKind::Comment => {
                self.offset += raw_token.len;
                Token::new(TokenKind::Comment, raw_token.len)
            }
            RawTokenKind::Newline => {
                self.offset += raw_token.len;
                Token::new(TokenKind::Newline, raw_token.len)
            }
            RawTokenKind::Whitespace => {
                self.offset += raw_token.len;
                Token::new(TokenKind::Whitespace, raw_token.len)
            }
            RawTokenKind::Ident => {
                self.offset += raw_token.len;
                Token::new(TokenKind::lookup_ident(literal), raw_token.len)
            }
            RawTokenKind::Float => {
                self.offset += raw_token.len;
                Token::new(TokenKind::Literal(token::LiteralKind::Float), raw_token.len)
            }
            RawTokenKind::Int => {
                self.offset += raw_token.len;
                Token::new(TokenKind::Literal(token::LiteralKind::Int), raw_token.len)
            }
            RawTokenKind::String => {
                self.offset += raw_token.len;
                Token::new(TokenKind::Literal(token::LiteralKind::Str), raw_token.len)
            }
            RawTokenKind::Symbol => {
                let current_char = literal.chars().next().unwrap();
                let next_char = self.scanner.cursor.first();

                let mut buffer = [0u8; 8];

                let first_part = current_char.encode_utf8(&mut buffer);
                let mut total_len = first_part.len();

                if next_char != '\0' {
                    let second_part = next_char.encode_utf8(&mut buffer[total_len..]);
                    total_len += second_part.len();
                }

                let combined_slice =
                    unsafe { core::str::from_utf8_unchecked(&buffer[..total_len]) };

                let combined_kind = TokenKind::lookup_symbol(combined_slice);

                if combined_kind != TokenKind::Unknown && combined_slice.len() > literal.len() {
                    self.scanner.cursor.bump();
                    let token_len = combined_slice.len();
                    self.offset += token_len;
                    Token::new(combined_kind, token_len)
                } else {
                    let single_kind = TokenKind::lookup_symbol(literal);
                    let token_len = literal.len();
                    self.offset += token_len;
                    Token::new(single_kind, token_len)
                }
            }
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token();

        if let TokenKind::Eof = token.kind {
            None
        } else {
            Some(token)
        }
    }
}
