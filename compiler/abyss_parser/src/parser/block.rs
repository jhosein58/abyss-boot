use abyss_lexer::token::TokenKind;

use crate::{ast::Stmt, parser::Parser};

impl<'a> Parser<'a> {
    pub fn parse_block(&mut self) -> Option<Vec<Stmt>> {
        self.consume(TokenKind::OBrace)?;

        self.optional(TokenKind::Newline);

        let mut scope = Vec::new();

        while !self.stream.is(TokenKind::CBrace) && !self.stream.is(TokenKind::Eof) {
            let Some(stmt) = self.parse_stmt(&mut scope) else {
                continue;
            };
            scope.push(stmt);
        }

        self.consume(TokenKind::CBrace)?;

        Some(scope)
    }
}
