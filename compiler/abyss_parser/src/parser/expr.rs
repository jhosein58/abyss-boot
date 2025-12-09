// use abyss_lexer::token::TokenKind;

// use crate::{
//     ast::{Expr, LetExpr, Lit, ReturnExpr},
//     parser::Parser,
// };

// impl<'a> Parser<'a> {
//     pub fn parse_expr(&mut self) -> Option<Expr> {
//         match self.stream.current().kind {
//             TokenKind::Let => Expr::Let(self.parse_let_expr()?).into(),
//             TokenKind::Ret => Expr::Return(self.parse_return_expr()?).into(),
//             _ => {
//                 self.advance();
//                 Expr::Err.into()
//             }
//         }
//     }

//     pub fn parse_return_expr(&mut self) -> Option<ReturnExpr> {
//         let span = self.stream.current_span();
//         self.advance();

//         let val = self.stream.current_lit().to_string();
//         self.advance();

//         self.consume(TokenKind::Newline)?;

//         Some(ReturnExpr {
//             value: Some(Box::new(Expr::Lit(Lit::Int(val.parse().unwrap())))),
//             span,
//         })
//     }

//     pub fn parse_let_expr(&mut self) -> Option<LetExpr> {
//         self.consume(TokenKind::Let)?;

//         None
//     }
// }
