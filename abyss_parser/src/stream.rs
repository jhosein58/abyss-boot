use abyss_lexer::{
    lexer::Lexer,
    token::{Token, TokenKind},
};

use crate::source_map::Span;

pub struct TokenStream<'a> {
    source: &'a str,
    lexer: Lexer<'a>,
    current: Token,
    offset: usize,
    peek: Token,
    peek_offset: usize,
}

impl<'a> TokenStream<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut stream = Self {
            source,
            lexer: Lexer::new(source),
            current: Token::dummy(),
            offset: 0,
            peek: Token::dummy(),
            peek_offset: 0,
        };

        stream.advance();
        stream.advance();

        stream
    }

    pub fn advance(&mut self) {
        if self.peek.kind == TokenKind::Eof {
            self.current = self.peek.clone();
            self.offset = self.peek_offset;
            return;
        }

        self.current = self.peek.clone();
        self.offset = self.peek_offset;

        self.peek_offset = self.offset + self.current.len;
        self.peek = self.lexer.next_token();

        while Self::is_useless(&self.peek)
            || (self.peek.kind == TokenKind::Newline && self.current.kind == TokenKind::Newline)
        {
            if self.peek.kind == TokenKind::Eof {
                break;
            }

            self.peek_offset += self.peek.len;
            self.peek = self.lexer.next_token();
        }
    }

    fn is_useless(token: &Token) -> bool {
        matches!(token.kind, TokenKind::Whitespace | TokenKind::Comment)
    }

    pub fn current(&self) -> &Token {
        &self.current
    }

    pub fn peek(&self) -> &Token {
        &self.peek
    }

    pub fn current_lit(&self) -> &str {
        &self.source[self.offset..self.offset + self.current.len]
    }

    pub fn peek_lit(&self) -> &str {
        &self.source[self.peek_offset..self.peek_offset + self.peek.len]
    }

    pub fn current_offset(&self) -> usize {
        self.offset
    }

    pub fn peek_offset(&self) -> usize {
        self.peek_offset
    }

    pub fn current_span(&self) -> Span {
        Span::new(self.offset, self.offset + self.current.len)
    }

    pub fn peek_span(&self) -> Span {
        Span::new(self.peek_offset, self.peek_offset + self.peek.len)
    }

    pub fn is_at_end(&self) -> bool {
        self.current.kind == TokenKind::Eof
    }

    pub fn is(&self, kind: TokenKind) -> bool {
        self.current.kind == kind
    }

    pub fn is_peek(&self, kind: TokenKind) -> bool {
        self.peek.kind == kind
    }

    pub fn consume(&mut self, kind: TokenKind) -> bool {
        if self.current.kind == kind {
            self.advance();
            true
        } else {
            false
        }
    }
}
