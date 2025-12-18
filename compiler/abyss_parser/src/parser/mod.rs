use abyss_lexer::token::TokenKind;
use colored::Colorize;
use std::fmt::Write;

use crate::{
    ast::{FunctionBody, FunctionDef, Program, Type},
    error::{ParseError, ParseErrorKind},
    source_map::{SourceMap, Span},
    stream::TokenStream,
};

pub mod block;
pub mod expr;
pub mod func;
pub mod stmt;

pub struct Parser<'a> {
    source: &'a str,
    stream: TokenStream<'a>,
    source_map: SourceMap,
    errors: Vec<ParseError>,
    recorded_span: Span,
    unique_id_counter: u32,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            source: input,
            stream: TokenStream::new(input),
            source_map: SourceMap::new(input),
            errors: Vec::new(),
            recorded_span: Span { start: 0, end: 0 },
            unique_id_counter: 0,
        }
    }

    pub fn advance(&mut self) {
        self.stream.advance();
    }

    pub fn mark_current_span(&mut self) {
        self.recorded_span = self.stream.current_span()
    }

    pub fn mark_peek_span(&mut self) {
        self.recorded_span = self.stream.peek_span()
    }

    pub fn emit_error(&mut self, kind: ParseErrorKind, span: Span) {
        let message = kind.to_string();
        self.errors.push(ParseError {
            kind,
            message,
            pos: span,
        });
    }

    pub fn emit_error_at_current(&mut self, kind: ParseErrorKind) {
        let span = self.stream.current_span();
        self.emit_error(kind, span);
    }

    pub fn emit_error_at_peek(&mut self, kind: ParseErrorKind) {
        let span = self.stream.peek_span();
        self.emit_error(kind, span);
    }

    pub fn errors(&self) -> &[ParseError] {
        &self.errors
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn format_errors(&self, filename: &str) -> String {
        let mut output = String::new();

        for error in &self.errors {
            let pos = self
                .source_map
                .position_from_span(&error.pos, self.source)
                .unwrap();

            let line_content = self.source.lines().nth(pos.line - 1).unwrap_or("");

            let _ = writeln!(
                &mut output,
                "{}: {}",
                "error".bright_red().bold(),
                error.message.bold()
            );

            let line_num_str = pos.line.to_string();
            let padding = " ".repeat(line_num_str.len());
            let _ = writeln!(
                &mut output,
                "{} {}{}:{}:{}",
                padding,
                "-->".blue().bold(),
                filename,
                pos.line,
                pos.column
            );

            let _ = writeln!(&mut output, "{} {}", padding, "|".blue().bold());

            let _ = writeln!(
                &mut output,
                "{} {} {}",
                line_num_str.blue().bold(),
                "|".blue().bold(),
                line_content
            );

            let col_padding = if pos.column > 0 {
                " ".repeat(pos.column - 1)
            } else {
                String::new()
            };

            let caret_len = if error.pos.len() > 0 {
                error.pos.len()
            } else {
                1
            };
            let carets = "^".repeat(caret_len);

            let _ = writeln!(
                &mut output,
                "{} {} {}{}",
                padding,
                "|".blue().bold(),
                col_padding,
                carets.bright_red().bold()
            );

            let _ = writeln!(&mut output, "");
        }

        output
    }

    pub fn parse_program(&mut self) -> Program {
        let mut functions = Self::get_std_externs();

        while self.stream.current().kind != TokenKind::Eof {
            if let Some(func) = self.parse_function() {
                functions.push(func);
            }
        }

        Program {
            modules: Vec::new(),
            structs: Vec::new(),
            functions,
            statics: Vec::new(),
            enums: Vec::new(),
        }
    }

    fn optional(&mut self, kind: TokenKind) {
        if self.stream.is(kind) {
            self.advance();
        }
    }

    pub fn synchronize(&mut self) {
        self.stream.advance();

        while !self.stream.is_at_end() {
            let kind = self.stream.current().kind;

            if kind == TokenKind::Newline {
                self.stream.advance();
                return;
            }

            match kind {
                TokenKind::Fn
                | TokenKind::Let
                | TokenKind::If
                | TokenKind::While
                | TokenKind::For
                | TokenKind::Ret => {
                    return;
                }

                TokenKind::CBrace => {
                    return;
                }

                _ => {}
            }

            self.stream.advance();
        }
    }

    fn consume(&mut self, expected: TokenKind) -> Option<()> {
        if !self.stream.consume(expected) {
            self.emit_error_at_current(ParseErrorKind::UnexpectedToken {
                expected,
                found: self.stream.current().kind,
            });
            self.synchronize();
            return None;
        }
        Some(())
    }

    fn get_unique_identifier(&mut self) -> String {
        let id = self.unique_id_counter;
        self.unique_id_counter += 1;
        format!("__internal_{}", id)
    }

    fn is(&mut self, kind: TokenKind) -> bool {
        self.stream.is(kind)
    }

    fn get_std_externs() -> Vec<FunctionDef> {
        let void_ptr = Type::Pointer(Box::new(Type::Void));
        let const_void_ptr = Type::Pointer(Box::new(Type::Void));
        let usize_t = Type::I64;
        let int_t = Type::I64;
        //let char_ptr = Type::Pointer(Box::new(Type::U8));

        vec![
            // FunctionDef {
            //     is_pub: true,
            //     name: "printf".to_string(),
            //     generics: vec![],
            //     params: vec![("format".to_string(), char_ptr.clone())],
            //     return_type: int_t.clone(),
            //     body: FunctionBody::Extern,
            // },
            FunctionDef {
                is_pub: true,
                name: "memset".to_string(),
                generics: vec![],
                params: vec![
                    ("s".to_string(), void_ptr.clone()),
                    ("c".to_string(), int_t.clone()),
                    ("n".to_string(), usize_t.clone()),
                ],
                return_type: void_ptr.clone(),
                body: FunctionBody::Extern,
            },
            FunctionDef {
                is_pub: true,
                name: "memcpy".to_string(),
                generics: vec![],
                params: vec![
                    ("dest".to_string(), void_ptr.clone()),
                    ("src".to_string(), const_void_ptr.clone()),
                    ("n".to_string(), usize_t.clone()),
                ],
                return_type: void_ptr.clone(),
                body: FunctionBody::Extern,
            },
            FunctionDef {
                is_pub: true,
                name: "malloc".to_string(),
                generics: vec![],
                params: vec![("size".to_string(), usize_t.clone())],
                return_type: void_ptr.clone(),
                body: FunctionBody::Extern,
            },
            FunctionDef {
                is_pub: true,
                name: "realloc".to_string(),
                generics: vec![],
                params: vec![
                    ("ptr".to_string(), void_ptr.clone()),
                    ("size".to_string(), usize_t.clone()),
                ],
                return_type: void_ptr.clone(),
                body: FunctionBody::Extern,
            },
            FunctionDef {
                is_pub: true,
                name: "free".to_string(),
                generics: vec![],
                params: vec![("ptr".to_string(), void_ptr.clone())],
                return_type: Type::Void,
                body: FunctionBody::Extern,
            },
            FunctionDef {
                is_pub: true,
                name: "exit".to_string(),
                generics: vec![],
                params: vec![("status".to_string(), int_t.clone())],
                return_type: Type::Void,
                body: FunctionBody::Extern,
            },
        ]
    }
}
