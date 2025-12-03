// use crate::{
//     lexer::{
//         lexer::Lexer,
//         token::{Token, TokenKind},
//     },
//     parser::ast::{Expression, Program, Statement},
// };

// pub struct Parser {
//     lexer: Lexer,
//     current_token: Token,
//     peek_token: Token,
//     pub all_tokens: Vec<String>,
// }

// impl Parser {
//     pub fn new(mut lexer: Lexer) -> Self {
//         let current_token = lexer.next_token();
//         let peek_token = lexer.next_token();

//         Parser {
//             lexer,
//             current_token,
//             peek_token,
//             all_tokens: Vec::new(),
//         }
//     }

//     // ---------------- Helpers ----------------
//     fn next_token(&mut self) {
//         self.all_tokens.push(format!(
//             "{:?}('{}')",
//             self.current_token.kind, self.current_token.literal
//         ));

//         self.current_token = self.peek_token.clone();
//         self.peek_token = self.lexer.next_token();
//     }

//     fn peek_token_is(&self, kind: TokenKind) -> bool {
//         std::mem::discriminant(&self.peek_token.kind) == std::mem::discriminant(&kind)
//     }

//     fn current_token_is(&self, kind: TokenKind) -> bool {
//         std::mem::discriminant(&self.current_token.kind) == std::mem::discriminant(&kind)
//     }

//     fn expect_peek(&mut self, kind: TokenKind) -> bool {
//         if self.peek_token_is(kind) {
//             self.next_token();
//             true
//         } else {
//             false
//         }
//     }

//     fn oe_stmt(option: Option<Statement>, line: usize) -> Option<Statement> {
//         option
//             .expect(format!("ERROR in line: {}", line).as_str())
//             .into()
//     }

//     fn _oe_expr(option: Option<Expression>, line: usize) -> Option<Expression> {
//         option
//             .expect(format!("ERROR in line: {}", line).as_str())
//             .into()
//     }

//     fn _is_allowed_operator(op: &str) -> bool {
//         match op {
//             "+" | "-" | "*" | "/" => true,
//             _ => false,
//         }
//     }

//     // ---------------- Entry Point ----------------
//     pub fn parse_program(&mut self) -> Program {
//         let mut statements = vec![];

//         while self.current_token.kind != TokenKind::EOF {
//             if self.current_token.kind == TokenKind::Newline {
//                 self.next_token();
//                 continue;
//             }

//             if let Some(stmt) = self.parse_statement() {
//                 statements.push(stmt);
//             }

//             self.next_token();
//         }

//         Program { statements }
//     }

//     // ---------------- Statement Parsing ----------------
//     fn parse_statement(&mut self) -> Option<Statement> {
//         match self.current_token.kind {
//             TokenKind::Let => Self::oe_stmt(self.parse_let_statement(), self.current_token.line),
//             TokenKind::Fn => {
//                 Self::oe_stmt(self.parse_function_definition(), self.current_token.line)
//             }

//             TokenKind::Ident => {
//                 if self.peek_token_is(TokenKind::Assign) {
//                     Self::oe_stmt(self.parse_assign_statement(), self.current_token.line)
//                 } else {
//                     Some(Statement::Expression(self.parse_expression()))
//                 }
//             }

//             TokenKind::Ret => self.parse_return_statement(),

//             //  _ => self.parse_expression_statement(),
//             _ => None,
//         }
//     }

//     fn parse_let_statement(&mut self) -> Option<Statement> {
//         if !self.expect_peek(TokenKind::Ident) {
//             return None;
//         }

//         let ident = self.current_token.literal.clone();

//         if !self.expect_peek(TokenKind::Assign) {
//             return None;
//         }

//         self.next_token();
//         let value_expr = self.parse_expression();

//         Some(Statement::Let {
//             name: ident,
//             value: value_expr,
//         })
//     }

//     fn parse_assign_statement(&mut self) -> Option<Statement> {
//         let ident = self.current_token.clone();
//         self.next_token();
//         self.next_token();

//         let value_expr = self.parse_expression();

//         Some(Statement::Assign {
//             name: ident.literal,
//             value: value_expr,
//         })
//     }

//     fn parse_function_definition(&mut self) -> Option<Statement> {
//         self.next_token();
//         let name = self.current_token.clone();
//         if !self.expect_peek(TokenKind::OParen) {
//             return None;
//         }
//         if !self.expect_peek(TokenKind::CParen) {
//             return None;
//         }
//         if !self.expect_peek(TokenKind::OBrace) {
//             return None;
//         }
//         let mut body = Vec::new();
//         while !self.current_token_is(TokenKind::CBrace) {
//             self.next_token();
//             if let Some(statement) = self.parse_statement() {
//                 body.push(statement);
//             }
//         }
//         Some(Statement::Function {
//             name: name.literal,
//             params: vec![],
//             body,
//         })
//     }

//     fn parse_return_statement(&mut self) -> Option<Statement> {
//         self.next_token();
//         Some(Statement::Return(self.parse_expression()))
//     }

//     // ---------------- Expression Parsing ----------------

//     fn parse_current_expr(&mut self) -> Expression {
//         if self.current_token_is(TokenKind::String) {
//             return Expression::String(self.current_token.literal.to_string());
//         } else if self.current_token_is(TokenKind::Number) {
//             return Expression::Number(self.current_token.literal.parse().unwrap());
//         } else {
//             if self.peek_token_is(TokenKind::OParen) {
//                 return self.parse_function_call_expression();
//             } else {
//                 return Expression::Identifier(self.current_token.literal.to_string());
//             }
//         }
//     }

//     fn parse_expression(&mut self) -> Expression {
//         let left = self.parse_current_expr();

//         if self.peek_token_is(TokenKind::Newline)
//             || self.peek_token_is(TokenKind::Comma)
//             || self.peek_token_is(TokenKind::CParen)
//         {
//             return left;
//         }

//         self.next_token();

//         let operator = self.current_token.literal.clone();
//         self.next_token();
//         let right = self.parse_expression();

//         Expression::Infix {
//             left: Box::new(left),
//             operator,
//             right: Box::new(right),
//         }
//     }

//     fn parse_function_call_expression(&mut self) -> Expression {
//         let ident = self.current_token.literal.clone();
//         self.next_token();

//         let mut args = vec![];

//         if self.peek_token_is(TokenKind::CParen) {
//             self.next_token();
//             return Expression::Call {
//                 function: Box::new(Expression::Identifier(ident)),
//                 arguments: args,
//             };
//         }

//         while !self.current_token_is(TokenKind::CParen) {
//             self.next_token();
//             args.push(self.parse_expression());
//             self.next_token();
//         }

//         Expression::Call {
//             function: Box::new(Expression::Identifier(ident)),
//             arguments: args,
//         }
//     }
// }
