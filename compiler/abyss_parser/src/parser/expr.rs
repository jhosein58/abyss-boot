use std::str::FromStr;

use abyss_lexer::token::{LiteralKind, TokenKind};

use crate::{
    ast::{BinaryOp, Expr, Lit, UnaryOp},
    error::ParseErrorKind,
    parser::Parser,
};

#[derive(PartialEq, PartialOrd, Clone, Copy)]
enum Precedence {
    None = 0,
    Term = 1,       // + -
    Factor = 2,     // * / %
    Comparison = 3, // < > <= >=
    Equality = 4,   // == !=
    And = 5,        // and
    Or = 6,         // or
    Unary = 7,      // - ! not
}

impl<'a> Parser<'a> {
    pub fn parse_expr(&mut self) -> Option<Expr> {
        self.parse_expr_bp(Precedence::None)
    }

    fn parse_expr_bp(&mut self, min_bp: Precedence) -> Option<Expr> {
        let mut lhs = self.parse_prefix()?;

        loop {
            let current_kind = self.stream.current().kind;
            let current_bp = self.get_binding_power(current_kind);

            if current_bp <= min_bp {
                break;
            }

            self.advance();
            let op = self.token_to_binary_op(current_kind);

            let rhs = self.parse_expr_bp(current_bp)?;
            lhs = Expr::Binary(Box::new(lhs), op, Box::new(rhs));
        }

        Some(lhs)
    }

    fn parse_prefix(&mut self) -> Option<Expr> {
        let token = self.stream.current();

        match token.kind {
            TokenKind::Literal(LiteralKind::Int) => {
                let val = self.parse_current_lit::<i64>()?;
                self.advance();
                Some(Expr::Lit(Lit::Int(val)))
            }
            TokenKind::Literal(LiteralKind::Float) => {
                let val = self.parse_current_lit::<f64>()?;
                self.advance();
                Some(Expr::Lit(Lit::Float(val)))
            }
            TokenKind::Ident => self.parse_ident_or_call(),
            TokenKind::OParen => {
                self.advance();
                let expr = self.parse_expr()?;
                self.consume(TokenKind::CParen);
                Some(expr)
            }

            TokenKind::Minus | TokenKind::Not => {
                let op = match token.kind {
                    TokenKind::Minus => UnaryOp::Neg,
                    TokenKind::Not => UnaryOp::Not,
                    _ => unreachable!(),
                };
                self.advance();
                let rhs = self.parse_expr_bp(Precedence::Unary)?;
                Some(Expr::Unary(op, Box::new(rhs)))
            }
            _ => {
                self.emit_error_at_current(ParseErrorKind::UnexpectedToken {
                    expected: TokenKind::Unknown,
                    found: token.kind,
                });
                None
            }
        }
    }

    fn parse_ident_or_call(&mut self) -> Option<Expr> {
        let name = self.stream.current_lit().to_string();
        self.advance();

        if self.stream.is(TokenKind::OParen) {
            self.advance();
            let mut args = Vec::new();

            if !self.stream.is(TokenKind::CParen) {
                loop {
                    if let Some(arg) = self.parse_expr() {
                        args.push(arg);
                    }

                    if self.stream.is(TokenKind::Comma) {
                        self.advance();
                    } else {
                        break;
                    }
                }
            }

            self.consume(TokenKind::CParen);
            return Some(Expr::Call(name, args));
        }

        Some(Expr::Ident(name))
    }

    fn parse_current_lit<T: FromStr>(&mut self) -> Option<T> {
        let span = self.stream.current_span();
        let text = &self.source[span.start..span.end];

        match text.parse::<T>() {
            Ok(v) => Some(v),
            Err(_) => None,
        }
    }

    fn get_binding_power(&self, kind: TokenKind) -> Precedence {
        match kind {
            TokenKind::Or => Precedence::Or,
            TokenKind::And => Precedence::And,

            TokenKind::EqEq | TokenKind::BangEq => Precedence::Equality,

            TokenKind::Lt | TokenKind::Gt | TokenKind::LtEq | TokenKind::GtEq => {
                Precedence::Comparison
            }

            TokenKind::Plus | TokenKind::Minus => Precedence::Term,

            TokenKind::Star | TokenKind::Slash | TokenKind::Percent => Precedence::Factor,

            _ => Precedence::None,
        }
    }

    fn token_to_binary_op(&self, kind: TokenKind) -> BinaryOp {
        match kind {
            TokenKind::Plus => BinaryOp::Add,
            TokenKind::Minus => BinaryOp::Sub,
            TokenKind::Star => BinaryOp::Mul,
            TokenKind::Slash => BinaryOp::Div,
            TokenKind::Percent => BinaryOp::Mod,
            TokenKind::EqEq => BinaryOp::Eq,
            TokenKind::BangEq => BinaryOp::Neq,
            TokenKind::Lt => BinaryOp::Lt,
            TokenKind::Gt => BinaryOp::Gt,
            TokenKind::LtEq => BinaryOp::Lte,
            TokenKind::GtEq => BinaryOp::Gte,
            TokenKind::And => BinaryOp::And,
            TokenKind::Or => BinaryOp::Or,
            _ => panic!("Not a binary operator: {:?}", kind),
        }
    }
}
