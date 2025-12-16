// use crate::{
//     ast::{BinaryOp, Expr, Lit, Stmt, Type},
//     parser::Parser,
// };
// use abyss_lexer::token::TokenKind as Tk;

// impl<'a> Parser<'a> {
//     pub fn parse_stmt(&mut self, scope: &mut Vec<Stmt>) -> Option<Stmt> {
//         let stmt = match self.stream.current().kind {
//             Tk::Let => self.parse_let_stmt()?,
//             Tk::Ret => self.parse_ret_stmt()?,
//             Tk::If => self.parse_if_stmt(scope)?,
//             Tk::While => self.parse_while_stmt()?,
//             Tk::For => self.parse_for_stmt(scope)?,

//             Tk::Out => self.parse_out_stmt()?,
//             Tk::Next => self.parse_next_stmt()?,

//             _ => self.parse_assignment_or_expr_stmt()?,
//         };

//         self.optional(Tk::Semi);
//         self.optional(Tk::Newline);
//         Some(stmt)
//     }

//     fn parse_assignment_or_expr_stmt(&mut self) -> Option<Stmt> {
//         let lhs_expr = self.parse_expr()?;

//         if self.stream.is(Tk::Assign) {
//             self.advance();
//             let rhs_expr = self.parse_expr()?;
//             return Some(Stmt::Assign(lhs_expr, rhs_expr));
//         }

//         if self.stream.is(Tk::Plus) && self.stream.is_peek(Tk::Assign) {
//             self.advance();
//             self.advance();
//             let rhs = self.parse_expr()?;
//             return Some(Stmt::Assign(
//                 lhs_expr.clone(),
//                 Expr::Binary(Box::new(lhs_expr), BinaryOp::Add, Box::new(rhs)),
//             ));
//         }
//         if self.stream.is(Tk::Minus) && self.stream.is_peek(Tk::Assign) {
//             self.advance();
//             self.advance();
//             let rhs = self.parse_expr()?;
//             return Some(Stmt::Assign(
//                 lhs_expr.clone(),
//                 Expr::Binary(Box::new(lhs_expr), BinaryOp::Sub, Box::new(rhs)),
//             ));
//         }

//         Some(Stmt::Expr(lhs_expr))
//     }

//     fn parse_let_stmt(&mut self) -> Option<Stmt> {
//         self.consume(Tk::Let)?;

//         let name = self.consume_ident()?;

//         let explicit_type = if self.stream.is(Tk::Colon) {
//             self.advance();
//             Some(self.parse_type()?)
//         } else {
//             None
//         };

//         let expr = if self.stream.is(Tk::Assign) {
//             self.advance();
//             Some(self.parse_expr()?)
//         } else {
//             None
//         };

//         let ty = match (&explicit_type, &expr) {
//             (Some(t), _) => t.clone(),
//             (None, Some(e)) => self.infer_type_from_expr(e),
//             _ => Type::Void,
//         };

//         Some(Stmt::Let(name, ty, expr))
//     }

//     fn infer_type_from_expr(&self, expr: &Expr) -> Type {
//         match expr {
//             Expr::Lit(lit) => self.infer_type_from_lit(lit),

//             Expr::Binary(lhs, op, rhs) => {
//                 let l = self.infer_type_from_expr(lhs);
//                 let r = self.infer_type_from_expr(rhs);

//                 match op {
//                     BinaryOp::Eq
//                     | BinaryOp::Neq
//                     | BinaryOp::Lt
//                     | BinaryOp::Gt
//                     | BinaryOp::Lte
//                     | BinaryOp::Gte
//                     | BinaryOp::And
//                     | BinaryOp::Or => Type::Bool,

//                     _ => {
//                         if l == Type::F64 || r == Type::F64 {
//                             Type::F64
//                         } else {
//                             Type::I64
//                         }
//                     }
//                 }
//             }

//             Expr::Unary(_, e) => self.infer_type_from_expr(e),

//             Expr::Cast(_, ty) => ty.clone(),

//             Expr::Index(arr, _) => {
//                 if let Type::Array(elem, _) = self.infer_type_from_expr(arr) {
//                     *elem
//                 } else {
//                     Type::Void
//                 }
//             }

//             Expr::Call(_, _) => Type::Void,
//             Expr::Ident(_) => Type::Void,
//             _ => Type::Void,
//         }
//     }

//     fn infer_type_from_lit(&self, lit: &Lit) -> Type {
//         match lit {
//             Lit::Int(_) => Type::I64,
//             Lit::Float(_) => Type::F64,
//             Lit::Bool(_) => Type::Bool,
//             Lit::Str(_) => Type::Array(Box::new(Type::U8), 0),
//             Lit::Array(elems) => {
//                 if let Some(first) = elems.first() {
//                     let elem_ty = self.infer_type_from_expr(first);
//                     Type::Array(Box::new(elem_ty), elems.len())
//                 } else {
//                     Type::Array(Box::new(Type::Void), 0)
//                 }
//             }
//         }
//     }

//     fn parse_ret_stmt(&mut self) -> Option<Stmt> {
//         self.consume(Tk::Ret)?;
//         let expr = self.parse_expr()?;
//         Some(Stmt::Ret(expr))
//     }

//     fn parse_if_stmt(&mut self, scope: &mut Vec<Stmt>) -> Option<Stmt> {
//         self.consume(Tk::If)?;

//         let condition = self.parse_expr()?;

//         let then_branch = self.parse_block()?;

//         let else_branch = if self.stream.is(Tk::Else) {
//             self.advance();

//             if self.stream.is(Tk::If) {
//                 let nested_if = self.parse_stmt(scope)?;
//                 Some(vec![nested_if])
//             } else {
//                 Some(self.parse_block()?)
//             }
//         } else {
//             None
//         };

//         Some(Stmt::If(condition, then_branch, else_branch))
//     }

//     fn parse_while_stmt(&mut self) -> Option<Stmt> {
//         self.consume(Tk::While)?;
//         let condition = self.parse_expr()?;
//         let body = self.parse_block()?;
//         Some(Stmt::While(condition, body))
//     }

//     fn consume_ident(&mut self) -> Option<String> {
//         if self.stream.is(Tk::Ident) {
//             let span = self.stream.current_span();
//             let name = self.source[span.start..span.end].to_string();
//             self.advance();
//             Some(name)
//         } else {
//             None
//         }
//     }
//     fn parse_for_stmt(&mut self, scope: &mut Vec<Stmt>) -> Option<Stmt> {
//         self.consume(Tk::For)?;

//         if self.stream.is(Tk::Ident) && self.stream.is_peek(Tk::In) {
//             let ident = self.consume_ident()?;
//             self.consume(Tk::In)?;
//             let start = self.parse_expr()?;
//             self.consume(Tk::RArrow)?;
//             let end = self.parse_expr()?;
//             let i_type = self.infer_type_from_expr(&start);

//             scope.push(Stmt::Let(ident.clone(), i_type.clone(), Some(start)));

//             let mut body = self.parse_block()?;
//             body.push(Stmt::Assign(
//                 Expr::Ident(ident.clone()),
//                 Expr::Binary(
//                     Box::new(Expr::Ident(ident.clone())),
//                     BinaryOp::Add,
//                     Box::new(Expr::Lit(Lit::Int(1))),
//                 ),
//             ));

//             return Some(Stmt::While(
//                 Expr::Binary(Box::new(Expr::Ident(ident)), BinaryOp::Lt, Box::new(end)),
//                 body,
//             ));
//         } else {
//             let ident = self.get_unique_identifier();
//             let end = self.parse_expr()?;

//             let start_expr = Expr::Lit(Lit::Int(0));
//             let i_type = Type::I64;

//             scope.push(Stmt::Let(ident.clone(), i_type.clone(), Some(start_expr)));

//             let end_ident = self.get_unique_identifier();
//             // let end_type = self.infer_type_from_expr(&end);
//             scope.push(Stmt::Let(end_ident.clone(), i_type, Some(end)));

//             let mut body = self.parse_block()?;

//             body.push(Stmt::Assign(
//                 Expr::Ident(ident.clone()),
//                 Expr::Binary(
//                     Box::new(Expr::Ident(ident.clone())),
//                     BinaryOp::Add,
//                     Box::new(Expr::Lit(Lit::Int(1))),
//                 ),
//             ));

//             return Some(Stmt::While(
//                 Expr::Binary(
//                     Box::new(Expr::Ident(ident)),
//                     BinaryOp::Lt,
//                     Box::new(Expr::Ident(end_ident)),
//                 ),
//                 body,
//             ));
//         }
//     }
//     pub fn parse_out_stmt(&mut self) -> Option<Stmt> {
//         self.consume(Tk::Out)?;

//         Some(Stmt::Break)
//     }
//     pub fn parse_next_stmt(&mut self) -> Option<Stmt> {
//         self.consume(Tk::Next)?;
//         Some(Stmt::Continue)
//     }
// }
