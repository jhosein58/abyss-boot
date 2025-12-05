use abyss_lexer::token::TokenKind;

use crate::{ast::Function, error::ParseErrorKind, parser::Parser};

impl<'a> Parser<'a> {
    pub fn parse_function(&mut self) -> Option<Function> {
        self.stream.consume(TokenKind::Newline);

        self.mark_current_span();

        if !self.stream.consume(TokenKind::Fn) {
            self.emit_error_at_current(ParseErrorKind::UnexpectedToken {
                expected: TokenKind::Fn,
                found: self.stream.current().kind,
            });
            self.stream.advance();
            return None;
        }

        self.stream.advance();
        None
    }
}
