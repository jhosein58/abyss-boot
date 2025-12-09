use abyss_lexer::token::TokenKind;

use crate::{ast::Function, error::ParseErrorKind, parser::Parser};

impl<'a> Parser<'a> {
    fn synchronize_func(&mut self) {
        self.stream.advance();

        while !self.stream.is_at_end() {
            match self.stream.current().kind {
                TokenKind::Fn => {
                    return;
                }

                _ => {}
            }

            self.stream.advance();
        }
    }

    fn consume_func(&mut self, expected: TokenKind) -> Option<()> {
        if !self.stream.consume(expected) {
            self.emit_error_at_current(ParseErrorKind::UnexpectedToken {
                expected: expected,
                found: self.stream.current().kind,
            });
            self.synchronize_func();
            return None;
        }
        Some(())
    }

    fn expect(&mut self, expected: TokenKind) -> Option<()> {
        if !self.stream.is(expected) {
            self.emit_error_at_current(ParseErrorKind::UnexpectedToken {
                expected: expected,
                found: self.stream.current().kind,
            });
            self.synchronize_func();
            return None;
        }
        Some(())
    }

    fn read_ident(&mut self) -> Option<String> {
        self.expect(TokenKind::Ident)?;
        let ident = self.stream.current_lit().to_string();

        self.advance();
        Some(ident)
    }

    pub fn parse_function(&mut self) -> Option<Function> {
        if self.stream.is_peek(TokenKind::Eof) {
            self.advance();
            return None;
        }

        self.stream.consume(TokenKind::Newline);

        self.consume_func(TokenKind::Fn)?;

        let function_ident = self.read_ident()?;

        let args = self.parse_fucn_args()?;

        let return_type = self.parse_return_type();
        self.optional(TokenKind::Newline);

        let body = if let Some(body) = self.parse_block() {
            body
        } else {
            self.synchronize_func();
            return None;
        };

        Some(Function {
            name: function_ident,
            params: args,
            return_type,
            body,
        })
    }

    fn parse_type(&mut self) -> Option<String> {
        let ty_ident = self.read_ident()?;
        Some(ty_ident)
    }

    pub fn parse_fucn_args(&mut self) -> Option<Vec<(String, String)>> {
        let mut args = Vec::new();

        self.consume_func(TokenKind::OParen)?;

        if self.stream.is(TokenKind::CParen) {
            self.advance();
            return Some(args);
        }

        while self.stream.is(TokenKind::Ident) {
            let arg_ident = self.read_ident()?;

            self.consume_func(TokenKind::Colon)?;

            args.push((arg_ident, self.parse_type()?));

            if self.stream.is(TokenKind::Comma) {
                self.advance();
            }
        }

        self.consume_func(TokenKind::CParen)?;

        Some(args)
    }

    pub fn parse_return_type(&mut self) -> Option<String> {
        if self.stream.is(TokenKind::Colon) {
            self.advance();
            self.parse_type()
        } else {
            None
        }
    }
}
