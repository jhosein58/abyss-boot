use crate::{
    ast::{BinaryOp, Expr, Lit, Stmt},
    parser::Parser,
};
use abyss_lexer::token::TokenKind as Tk;

impl<'a> Parser<'a> {
    pub fn parse_stmt(&mut self, scope: &mut Vec<Stmt>) -> Option<Stmt> {
        let token = self.stream.current();

        let stmt = match token.kind {
            Tk::Let => self.parse_let_stmt()?,
            Tk::Ret => self.parse_ret_stmt()?,
            Tk::If => self.parse_if_stmt(scope)?,
            Tk::While => self.parse_while_stmt()?,
            Tk::For => self.parse_for_stmt(scope)?,
            Tk::Ident => {
                if self.stream.is_peek(Tk::Assign)
                    || self.stream.is_peek(Tk::Plus)
                    || self.stream.is_peek(Tk::Minus)
                {
                    self.parse_assign_stmt()?
                } else {
                    self.parse_expr_stmt()?
                }
            }

            _ => self.parse_expr_stmt()?,
        };

        self.optional(Tk::Semi);
        self.optional(Tk::Newline);

        Some(stmt)
    }

    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        self.consume(Tk::Let)?;

        let name = self.consume_ident()?;

        self.consume(Tk::Assign)?;

        let expr = self.parse_expr()?;

        Some(Stmt::Let(name, expr))
    }

    fn parse_assign_stmt(&mut self) -> Option<Stmt> {
        let name = self.consume_ident()?;

        if self.stream.is(Tk::Plus) || self.stream.is(Tk::Minus) {
            let op = if self.stream.is(Tk::Plus) {
                BinaryOp::Add
            } else {
                BinaryOp::Sub
            };
            self.advance();

            self.consume(Tk::Assign)?;
            let expr = self.parse_expr()?;
            return Some(Stmt::Assign(
                name.clone(),
                Expr::Binary(Box::new(Expr::Ident(name)), op, Box::new(expr)),
            ));
        }
        self.consume(Tk::Assign)?;
        let expr = self.parse_expr()?;

        Some(Stmt::Assign(name, expr))
    }

    fn parse_ret_stmt(&mut self) -> Option<Stmt> {
        self.consume(Tk::Ret)?;
        let expr = self.parse_expr()?;
        Some(Stmt::Ret(expr))
    }

    fn parse_if_stmt(&mut self, scope: &mut Vec<Stmt>) -> Option<Stmt> {
        self.consume(Tk::If)?;

        let condition = self.parse_expr()?;

        let then_branch = self.parse_block()?;

        let else_branch = if self.stream.is(Tk::Else) {
            self.advance();

            if self.stream.is(Tk::If) {
                let nested_if = self.parse_stmt(scope)?;
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
        self.consume(Tk::While)?;
        let condition = self.parse_expr()?;
        let body = self.parse_block()?;
        Some(Stmt::While(condition, body))
    }

    fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        let expr = self.parse_expr()?;
        Some(Stmt::Expr(expr))
    }

    fn consume_ident(&mut self) -> Option<String> {
        if self.stream.is(Tk::Ident) {
            let span = self.stream.current_span();
            let name = self.source[span.start..span.end].to_string();
            self.advance();
            Some(name)
        } else {
            None
        }
    }

    fn parse_for_stmt(&mut self, scope: &mut Vec<Stmt>) -> Option<Stmt> {
        self.consume(Tk::For)?;
        if self.is(Tk::Ident) && self.stream.is_peek(Tk::In) {
            let ident = self.stream.current_lit().to_string();
            self.advance();
            self.advance();
            let start = self.parse_expr()?;
            self.consume(Tk::RArrow)?;
            let end = self.parse_expr()?;

            scope.push(Stmt::Let(ident.clone(), start));

            let end_ident = self.get_unique_identifier();
            scope.push(Stmt::Let(end_ident.clone(), end));

            let mut body = self.parse_block()?;

            body.push(Stmt::Assign(
                ident.clone(),
                Expr::Binary(
                    Box::new(Expr::Ident(ident.clone())),
                    BinaryOp::Add,
                    Box::new(Expr::Lit(Lit::Int(1))),
                ),
            ));

            scope.push(Stmt::While(
                Expr::Binary(
                    Box::new(Expr::Ident(ident)),
                    BinaryOp::Lt,
                    Box::new(Expr::Ident(end_ident)),
                ),
                body,
            ));
            self.advance();
        } else {
            let ident = self.get_unique_identifier();
            let expr = self.parse_expr()?;

            let mut body = self.parse_block()?;

            body.push(Stmt::Assign(
                ident.clone(),
                Expr::Binary(
                    Box::new(Expr::Ident(ident.clone())),
                    BinaryOp::Add,
                    Box::new(Expr::Lit(Lit::Int(1))),
                ),
            ));
            scope.push(Stmt::Let(ident.clone(), Expr::Lit(Lit::Int(0))));
            let end_ident = self.get_unique_identifier();
            scope.push(Stmt::Let(end_ident.clone(), expr));

            scope.push(Stmt::While(
                Expr::Binary(
                    Box::new(Expr::Ident(ident.clone())),
                    BinaryOp::Lt,
                    Box::new(Expr::Ident(end_ident)),
                ),
                body,
            ));

            self.advance();
        }

        None
    }
}
