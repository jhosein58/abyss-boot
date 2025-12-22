use std::str::FromStr;

use abyss_lexer::token::{LiteralKind, TokenKind};

use crate::{
    ast::{BinaryOp, Expr, Lit, Type, UnaryOp},
    error::ParseErrorKind,
    parser::Parser,
};

#[derive(PartialEq, PartialOrd, Clone, Copy, Debug)]
#[repr(u8)]
enum Precedence {
    _None = 0,
    _Assignment = 1,
    Or = 2,
    And = 3,
    BitwiseOr = 4,
    BitwiseXor = 5,
    BitwiseAnd = 6,
    Equality = 7,
    Comparison = 8,
    Shift = 9,
    Term = 10,
    Factor = 11,
    Unary = 12,
    Call = 13,
}

impl Precedence {
    fn get_next_power(&self) -> u8 {
        let val = *self as u8;
        match self {
            Self::_Assignment => val,
            _ => val + 1,
        }
    }
}

impl<'a> Parser<'a> {
    pub fn parse_expr(&mut self) -> Option<Expr> {
        self.parse_expr_bp(0)
    }

    fn parse_expr_bp(&mut self, min_bp: u8) -> Option<Expr> {
        self.skip_newlines();

        let mut lhs = self.parse_prefix()?;

        loop {
            self.skip_newlines();

            while let Some(postfix_prec) =
                self.get_postfix_binding_power(self.stream.current().kind)
            {
                if (postfix_prec as u8) < min_bp {
                    break;
                }
                lhs = self.parse_postfix(lhs)?;
            }

            let current_kind = self.stream.current().kind;

            if (current_kind == TokenKind::Plus || current_kind == TokenKind::Minus)
                && self.stream.is_peek(TokenKind::Assign)
            {
                break;
            }

            if let Some(infix_prec) = self.get_infix_binding_power(current_kind) {
                let infix_bp = infix_prec as u8;

                if infix_bp < min_bp {
                    break;
                }

                self.advance();
                let op = self.token_to_binary_op(current_kind);

                let next_min_bp = infix_prec.get_next_power();

                let rhs = match self.parse_expr_bp(next_min_bp) {
                    Some(e) => e,
                    None => break,
                };

                lhs = Expr::Binary(Box::new(lhs), op, Box::new(rhs));
            } else {
                break;
            }
        }

        Some(lhs)
    }

    fn parse_prefix(&mut self) -> Option<Expr> {
        self.skip_newlines();
        let token_kind = self.stream.current().kind;

        match token_kind {
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
            TokenKind::Literal(LiteralKind::Str) => {
                let val = self.stream.current_lit().to_string();
                self.advance();
                Some(Expr::Lit(Lit::Str(val)))
            }
            TokenKind::True => {
                self.advance();
                Some(Expr::Lit(Lit::Bool(true)))
            }
            TokenKind::False => {
                self.advance();
                Some(Expr::Lit(Lit::Bool(false)))
            }
            TokenKind::Ident => {
                let mut path = Vec::new();
                path.push(self.stream.current_lit().to_string());
                self.advance();

                while self.stream.is(TokenKind::ColonColon) {
                    self.advance();

                    if self.stream.is(TokenKind::Ident) {
                        path.push(self.stream.current_lit().to_string());
                        self.advance();
                    } else {
                        self.emit_error_at_current(ParseErrorKind::Expected(
                            "Identifier after '::'".to_string(),
                        ));
                        return None;
                    }
                }

                if self.stream.is(TokenKind::OBrace) {
                    let generics = Vec::new();
                    return self.parse_struct_literal(path, generics);
                }

                Some(Expr::Ident(path))
            }

            TokenKind::OParen => {
                self.advance();
                let expr = self.parse_expr()?;
                self.consume(TokenKind::CParen)?;
                Some(expr)
            }
            TokenKind::OBracket => {
                self.advance();
                self.parse_array_literal()
            }

            TokenKind::Size => {
                self.advance();
                self.consume(TokenKind::OParen)?;
                let target_type = self.parse_type()?;
                self.consume(TokenKind::CParen)?;
                Some(Expr::SizeOf(target_type))
            }

            TokenKind::Minus
            | TokenKind::Not
            | TokenKind::Tilde
            | TokenKind::Amp
            | TokenKind::Star => {
                let op = match token_kind {
                    TokenKind::Minus => UnaryOp::Neg,
                    TokenKind::Not => UnaryOp::Not,
                    TokenKind::Tilde => UnaryOp::BitNot,
                    TokenKind::Star => {
                        self.advance();
                        return Some(Expr::Deref(Box::new(
                            self.parse_expr_bp(Precedence::Unary as u8)?,
                        )));
                    }
                    TokenKind::Amp => {
                        self.advance();
                        return Some(Expr::AddrOf(Box::new(
                            self.parse_expr_bp(Precedence::Unary as u8)?,
                        )));
                    }
                    _ => unreachable!(),
                };
                self.advance();
                let rhs = self.parse_expr_bp(Precedence::Unary as u8)?;
                Some(Expr::Unary(op, Box::new(rhs)))
            }
            _ => {
                self.emit_error_at_current(ParseErrorKind::UnexpectedToken {
                    expected: TokenKind::Unknown,
                    found: token_kind,
                });
                None
            }
        }
    }

    fn get_postfix_binding_power(&self, kind: TokenKind) -> Option<Precedence> {
        Some(match kind {
            TokenKind::OParen | TokenKind::OBracket | TokenKind::As | TokenKind::Dot => {
                Precedence::Call
            }
            _ => return None,
        })
    }

    fn parse_postfix(&mut self, lhs: Expr) -> Option<Expr> {
        let token_kind = self.stream.current().kind;

        match token_kind {
            TokenKind::Dot => {
                self.advance();

                if !self.stream.is(TokenKind::Ident) {
                    self.emit_error_at_current(ParseErrorKind::Expected(
                        "Field or method name".to_string(),
                    ));
                    return None;
                }

                let name = self.stream.current_lit().to_string();
                self.advance();
                if self.stream.is(TokenKind::OParen) {
                    self.advance();

                    let mut args = Vec::new();
                    if !self.stream.is(TokenKind::CParen) {
                        loop {
                            args.push(self.parse_expr()?);
                            if !self.stream.consume(TokenKind::Comma) {
                                break;
                            }
                        }
                    }
                    self.consume(TokenKind::CParen)?;

                    Some(Expr::MethodCall(Box::new(lhs), name, args, Vec::new()))
                } else {
                    Some(Expr::Member(Box::new(lhs), name))
                }
            }

            TokenKind::OParen => {
                self.advance();
                let mut args = Vec::new();
                if !self.is(TokenKind::CParen) {
                    loop {
                        args.push(self.parse_expr()?);
                        if !self.stream.consume(TokenKind::Comma) {
                            break;
                        }
                    }
                }
                self.consume(TokenKind::CParen)?;
                Some(Expr::Call(Box::new(lhs), args, Vec::new()))
            }
            TokenKind::OBracket => {
                self.advance();
                let index_expr = self.parse_expr()?;
                self.consume(TokenKind::CBracket)?;
                Some(Expr::Index(Box::new(lhs), Box::new(index_expr)))
            }
            TokenKind::As => {
                self.advance();
                let target_type = self.parse_type()?;
                Some(Expr::Cast(Box::new(lhs), target_type))
            }
            _ => unreachable!(),
        }
    }

    fn parse_array_literal(&mut self) -> Option<Expr> {
        let mut elements = Vec::new();
        if self.is(TokenKind::CBracket) {
            self.advance();
            return Some(Expr::Lit(Lit::Array(elements)));
        }

        loop {
            elements.push(self.parse_expr()?);
            if !self.stream.consume(TokenKind::Comma) {
                break;
            }
        }
        self.consume(TokenKind::CBracket)?;
        Some(Expr::Lit(Lit::Array(elements)))
    }

    fn parse_current_lit<T: FromStr>(&mut self) -> Option<T> {
        let text = self.stream.current_lit();
        text.parse::<T>().ok()
    }

    fn get_infix_binding_power(&self, kind: TokenKind) -> Option<Precedence> {
        Some(match kind {
            TokenKind::Or => Precedence::Or,
            TokenKind::And => Precedence::And,
            TokenKind::Pipe => Precedence::BitwiseOr,
            TokenKind::Caret => Precedence::BitwiseXor,
            TokenKind::Amp => Precedence::BitwiseAnd,
            TokenKind::EqEq | TokenKind::BangEq => Precedence::Equality,
            TokenKind::Lt | TokenKind::Gt | TokenKind::LtEq | TokenKind::GtEq => {
                Precedence::Comparison
            }
            TokenKind::LeftShift | TokenKind::RightShift => Precedence::Shift,
            TokenKind::Plus | TokenKind::Minus => Precedence::Term,
            TokenKind::Star | TokenKind::Slash | TokenKind::Percent => Precedence::Factor,
            _ => return None,
        })
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
            TokenKind::Amp => BinaryOp::BitAnd,
            TokenKind::Pipe => BinaryOp::BitOr,
            TokenKind::Caret => BinaryOp::BitXor,
            TokenKind::LeftShift => BinaryOp::Shl,
            TokenKind::RightShift => BinaryOp::Shr,
            _ => panic!("Not a binary operator: {:?}", kind),
        }
    }

    pub fn parse_type(&mut self) -> Option<Type> {
        if self.stream.is(TokenKind::Amp) {
            self.advance();
            let inner_type = self.parse_type()?;
            return Some(Type::Pointer(Box::new(inner_type)));
        }

        let mut base_type = if self.stream.consume(TokenKind::U8) {
            Type::U8
        } else if self.stream.consume(TokenKind::I64) {
            Type::I64
        } else if self.stream.consume(TokenKind::F64) {
            Type::F64
        } else if self.stream.consume(TokenKind::Bool) {
            Type::Bool
        } else if self.stream.consume(TokenKind::Pass) {
            Type::Void
        } else if self.stream.is(TokenKind::Ident) {
            let name = self.stream.current_lit().to_string();
            self.advance();
            Type::Struct(vec![name], Vec::new())
        } else {
            self.emit_error_at_current(ParseErrorKind::Expected("type name".to_string()));
            return None;
        };

        loop {
            if self.stream.is(TokenKind::Star) {
                self.advance();
                base_type = Type::Pointer(Box::new(base_type));
            } else if self.stream.is(TokenKind::OBracket) {
                self.advance();
                if let TokenKind::Literal(LiteralKind::Int) = self.stream.current().kind {
                    if let Some(size) = self.parse_current_lit::<usize>() {
                        self.advance();
                        if self.stream.consume(TokenKind::CBracket) {
                            base_type = Type::Array(Box::new(base_type), size);
                            continue;
                        }
                    }
                }
                self.emit_error_at_current(ParseErrorKind::Expected("Array size".to_string()));
                return None;
            } else {
                break;
            }
        }

        Some(base_type)
    }

    fn parse_struct_literal(&mut self, path: Vec<String>, generics: Vec<Type>) -> Option<Expr> {
        self.consume(TokenKind::OBrace)?;

        let mut fields = Vec::new();

        while !self.stream.is(TokenKind::CBrace) && !self.stream.is_at_end() {
            self.skip_newlines();
            if self.stream.is(TokenKind::CBrace) {
                break;
            }

            if !self.stream.is(TokenKind::Ident) {
                self.emit_error_at_current(ParseErrorKind::Expected("Field name".to_string()));
                return None;
            }
            let field_name = self.stream.current_lit().to_string();
            self.advance();

            if !self.stream.consume(TokenKind::Colon) {
                self.emit_error_at_current(ParseErrorKind::Expected(
                    "Colon ':' after field name".to_string(),
                ));
                return None;
            }

            let value_expr = self.parse_expr()?;
            fields.push((field_name, value_expr));

            if !self.stream.consume(TokenKind::Comma) {
                if !self.stream.is(TokenKind::CBrace) {
                    self.emit_error_at_current(ParseErrorKind::Expected(
                        "Comma or closing brace".to_string(),
                    ));
                    return None;
                }
            }

            self.skip_newlines();
        }

        self.consume(TokenKind::CBrace)?;

        Some(Expr::StructInit(path, fields, generics))
    }
}
