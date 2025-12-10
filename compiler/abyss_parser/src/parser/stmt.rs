use crate::{ast::Stmt, parser::Parser};
use abyss_lexer::token::TokenKind;

impl<'a> Parser<'a> {
    pub fn parse_stmt(&mut self) -> Option<Stmt> {
        let token = self.stream.current();

        let stmt = match token.kind {
            TokenKind::Let => self.parse_let_stmt()?,
            TokenKind::Ret => self.parse_ret_stmt()?,
            TokenKind::If => self.parse_if_stmt()?,
            TokenKind::While => self.parse_while_stmt()?,
            TokenKind::Ident => {
                if self.stream.peek().kind == TokenKind::Assign {
                    self.parse_assign_stmt()?
                } else {
                    self.parse_expr_stmt()?
                }
            }

            _ => self.parse_expr_stmt()?,
        };

        self.optional(TokenKind::Semi);
        self.optional(TokenKind::Newline);

        Some(stmt)
    }

    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        self.consume(TokenKind::Let)?;

        let name = self.consume_ident()?;

        self.consume(TokenKind::Assign)?;

        let expr = self.parse_expr()?;

        Some(Stmt::Let(name, expr))
    }

    fn parse_assign_stmt(&mut self) -> Option<Stmt> {
        let name = self.consume_ident()?;
        self.consume(TokenKind::Assign)?;
        let expr = self.parse_expr()?;

        Some(Stmt::Assign(name, expr))
    }

    fn parse_ret_stmt(&mut self) -> Option<Stmt> {
        self.consume(TokenKind::Ret)?;
        let expr = self.parse_expr()?;
        Some(Stmt::Ret(expr))
    }

    fn parse_if_stmt(&mut self) -> Option<Stmt> {
        self.consume(TokenKind::If)?;

        let condition = self.parse_expr()?;

        let then_branch = self.parse_block()?;

        let else_branch = if self.stream.is(TokenKind::Else) {
            self.advance();

            if self.stream.is(TokenKind::If) {
                let nested_if = self.parse_stmt()?;
                Some(vec![nested_if])
            } else {
                Some(self.parse_block()?)
            }
        } else {
            None
        };

        Some(Stmt::If(condition, then_branch, else_branch))
    }

    fn parse_while_stmt(&mut self) -> Option<Stmt> {
        self.consume(TokenKind::While)?;
        let condition = self.parse_expr()?;
        let body = self.parse_block()?;
        Some(Stmt::While(condition, body))
    }

    fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        let expr = self.parse_expr()?;
        Some(Stmt::Expr(expr))
    }

    fn consume_ident(&mut self) -> Option<String> {
        if self.stream.is(TokenKind::Ident) {
            let span = self.stream.current_span();
            let name = self.source[span.start..span.end].to_string();
            self.advance();
            Some(name)
        } else {
            None
        }
    }
}
