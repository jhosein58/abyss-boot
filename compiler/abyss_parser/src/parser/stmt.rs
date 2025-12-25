use crate::{
    ast::{BinaryOp, Expr, Lit, Stmt, Type},
    parser::Parser,
};
use abyss_lexer::token::TokenKind as Tk;

impl<'a> Parser<'a> {
    pub fn parse_stmt(&mut self, scope: &mut Vec<Stmt>) -> Option<Stmt> {
        let stmt = match self.stream.current().kind {
            Tk::Let => self.parse_let_stmt()?,
            Tk::Fn => self.parse_nested_function()?,
            Tk::Ret => self.parse_ret_stmt()?,
            Tk::If => self.parse_if_stmt(scope)?,

            Tk::While => self.parse_while_stmt()?,
            Tk::For => self.parse_for_stmt(scope)?,
            Tk::Forever => self.parse_forever_stmt()?,

            Tk::Out => self.parse_out_stmt()?,
            Tk::Next => self.parse_next_stmt()?,

            _ => self.parse_assignment_or_expr_stmt()?,
        };

        self.optional(Tk::Semi);
        self.optional(Tk::Newline);
        Some(stmt)
    }

    fn parse_assignment_or_expr_stmt(&mut self) -> Option<Stmt> {
        let lhs_expr = self.parse_expr()?;

        if self.stream.is(Tk::Assign) {
            self.advance();
            let rhs_expr = self.parse_expr()?;
            return Some(Stmt::Assign(lhs_expr, rhs_expr));
        }

        if self.stream.is(Tk::Plus) && self.stream.is_peek(Tk::Assign) {
            self.advance();
            self.advance();
            let rhs = self.parse_expr()?;
            return Some(Stmt::Assign(
                lhs_expr.clone(),
                Expr::Binary(Box::new(lhs_expr), BinaryOp::Add, Box::new(rhs)),
            ));
        }
        if self.stream.is(Tk::Minus) && self.stream.is_peek(Tk::Assign) {
            self.advance();
            self.advance();
            let rhs = self.parse_expr()?;
            return Some(Stmt::Assign(
                lhs_expr.clone(),
                Expr::Binary(Box::new(lhs_expr), BinaryOp::Sub, Box::new(rhs)),
            ));
        }

        Some(Stmt::Expr(lhs_expr))
    }

    fn parse_nested_function(&mut self) -> Option<Stmt> {
        let func_def = self.parse_function(false)?;

        Some(Stmt::FunctionDef(Box::new(func_def)))
    }
    fn parse_forever_stmt(&mut self) -> Option<Stmt> {
        self.consume(Tk::Forever)?;

        let body_stmts = self.parse_block()?;

        Some(Stmt::While(
            Expr::Lit(Lit::Bool(true)),
            Box::new(Stmt::Block(body_stmts)),
        ))
    }

    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        self.consume(Tk::Let)?;

        let name = self.consume_ident()?;

        let explicit_type = if self.stream.is(Tk::Colon) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        let expr = if self.stream.is(Tk::Assign) {
            self.advance();
            Some(self.parse_expr()?)
        } else {
            None
        };

        Some(Stmt::Let(name, explicit_type, expr))
    }

    fn parse_ret_stmt(&mut self) -> Option<Stmt> {
        self.consume(Tk::Ret)?;
        let expr = self.parse_expr()?;
        Some(Stmt::Ret(expr))
    }

    fn parse_if_stmt(&mut self, scope: &mut Vec<Stmt>) -> Option<Stmt> {
        self.consume(Tk::If)?;

        let condition = self.parse_expr()?;

        let then_stmts = self.parse_block()?;
        let then_branch = Box::new(Stmt::Block(then_stmts));

        let else_branch = if self.stream.is(Tk::Else) {
            self.advance();

            if self.stream.is(Tk::If) {
                let nested_if = self.parse_stmt(scope)?;
                Some(Box::new(nested_if))
            } else {
                let else_stmts = self.parse_block()?;
                Some(Box::new(Stmt::Block(else_stmts)))
            }
        } else {
            None
        };

        Some(Stmt::If(condition, then_branch, else_branch))
    }

    fn parse_while_stmt(&mut self) -> Option<Stmt> {
        self.consume(Tk::While)?;
        let condition = self.parse_expr()?;
        let body_stmts = self.parse_block()?;
        Some(Stmt::While(condition, Box::new(Stmt::Block(body_stmts))))
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

    fn parse_for_stmt(&mut self, _: &mut Vec<Stmt>) -> Option<Stmt> {
        self.consume(Tk::For)?;

        if self.stream.is(Tk::Ident) && self.stream.is_peek(Tk::In) {
            let ident = self.consume_ident()?;
            self.consume(Tk::In)?;
            let start = self.parse_expr()?;
            self.consume(Tk::RArrow)?;
            let end = self.parse_expr()?;

            let i_type = Type::I64;

            let inc_stmt = Stmt::Assign(
                Expr::Ident(vec![ident.clone()]),
                Expr::Binary(
                    Box::new(Expr::Ident(vec![ident.clone()])),
                    BinaryOp::Add,
                    Box::new(Expr::Lit(Lit::Int(1))),
                ),
            );

            let mut body_stmts = vec![inc_stmt];
            body_stmts.extend(self.parse_block()?);

            return Some(Stmt::Block(vec![
                Stmt::Let(
                    ident.clone(),
                    Some(i_type),
                    Some(Expr::Binary(
                        Box::new(start),
                        BinaryOp::Sub,
                        Box::new(Expr::Lit(Lit::Int(1))),
                    )),
                ),
                Stmt::While(
                    Expr::Binary(
                        Box::new(Expr::Binary(
                            Box::new(Expr::Ident(vec![ident.clone()])),
                            BinaryOp::Add,
                            Box::new(Expr::Lit(Lit::Int(1))),
                        )),
                        BinaryOp::Lt,
                        Box::new(end),
                    ),
                    Box::new(Stmt::Block(body_stmts)),
                ),
            ]));
        } else {
            let ident = self.get_unique_identifier();
            let end = self.parse_expr()?;

            let start_expr = Expr::Lit(Lit::Int(-1));
            let i_type = Type::I64;

            let end_ident = self.get_unique_identifier();

            let inc_stmt = Stmt::Assign(
                Expr::Ident(vec![ident.clone()]),
                Expr::Binary(
                    Box::new(Expr::Ident(vec![ident.clone()])),
                    BinaryOp::Add,
                    Box::new(Expr::Lit(Lit::Int(1))),
                ),
            );

            let mut body_stmts = vec![inc_stmt];
            body_stmts.extend(self.parse_block()?);

            return Some(Stmt::Block(vec![
                Stmt::Let(ident.clone(), Some(i_type.clone()), Some(start_expr)),
                Stmt::Let(end_ident.clone(), Some(i_type), Some(end)),
                Stmt::While(
                    Expr::Binary(
                        Box::new(Expr::Binary(
                            Box::new(Expr::Ident(vec![ident])),
                            BinaryOp::Add,
                            Box::new(Expr::Lit(Lit::Int(1))),
                        )),
                        BinaryOp::Lt,
                        Box::new(Expr::Ident(vec![end_ident])),
                    ),
                    Box::new(Stmt::Block(body_stmts)),
                ),
            ]));
        }
    }

    pub fn parse_out_stmt(&mut self) -> Option<Stmt> {
        self.consume(Tk::Out)?;
        Some(Stmt::Break)
    }

    pub fn parse_next_stmt(&mut self) -> Option<Stmt> {
        self.consume(Tk::Next)?;
        Some(Stmt::Continue)
    }
}
