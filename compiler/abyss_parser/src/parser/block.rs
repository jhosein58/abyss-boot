use abyss_lexer::token::TokenKind;

use crate::{
    ast::{Expr, Lit, Stmt},
    parser::Parser,
};

impl<'a> Parser<'a> {
    pub fn parse_block(&mut self) -> Option<Vec<Stmt>> {
        self.consume(TokenKind::OBrace)?;

        self.optional(TokenKind::Newline);

        let mut scope = Vec::new();

        self.consume(TokenKind::Ret)?;
        let lit = self.stream.current_lit().to_string();
        self.advance();
        scope.push(Stmt::Ret(Expr::Lit(Lit::Int(lit.parse().unwrap()))));
        self.consume(TokenKind::Newline)?;

        // while !self.stream.is(TokenKind::CBrace) && !self.stream.is(TokenKind::Eof) {
        //     scope.push(self.parse_expr().unwrap_or(Expr::Err));
        // }

        self.consume(TokenKind::CBrace)?;

        Some(scope)
    }
}
