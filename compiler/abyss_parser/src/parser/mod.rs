use abyss_lexer::token::TokenKind;
use colored::Colorize;
use std::{
    collections::HashSet,
    fmt::Write,
    path::{Path, PathBuf},
};

use crate::{
    ast::{FunctionDef, Program, Stmt},
    error::{ParseError, ParseErrorKind},
    source_map::{SourceMap, Span},
    stream::TokenStream,
};

pub mod block;
pub mod expr;
pub mod func;
pub mod globals;
pub mod stmt;

pub struct Parser<'a> {
    pub source: &'a str,
    stream: TokenStream<'a>,
    source_map: SourceMap,
    errors: Vec<ParseError>,
    recorded_span: Span,
    unique_id_counter: u32,
    root_dir: PathBuf,
    loaded_paths: HashSet<PathBuf>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str, file_path: &str) -> Self {
        let path = Path::new(file_path);
        let root_dir = if path.is_file() {
            path.parent().unwrap_or(Path::new(".")).to_path_buf()
        } else {
            path.to_path_buf()
        };

        Self {
            source: input,
            stream: TokenStream::new(input),
            source_map: SourceMap::new(input),
            errors: Vec::new(),
            recorded_span: Span { start: 0, end: 0 },
            unique_id_counter: 0,
            root_dir,
            loaded_paths: HashSet::new(),
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
        self.parse_definitions(None)
    }

    fn parse_definitions(&mut self, end_token: Option<TokenKind>) -> Program {
        let mut functions = if end_token.is_none() {
            Self::get_std_externs()
        } else {
            Vec::new()
        };
        let mut structs = Vec::new();
        let mut statics = Vec::new();
        let mut modules = Vec::new();
        let mut uses = Vec::new();

        loop {
            self.skip_newlines();

            if self.stream.is_at_end() {
                break;
            }

            if let Some(end) = end_token {
                if self.stream.is(end) {
                    break;
                }
            }

            let is_pub = if self.stream.is(TokenKind::Pub) {
                self.advance();
                true
            } else {
                false
            };

            match self.stream.current().kind {
                TokenKind::Fn => {
                    if let Some(func) = self.parse_function(is_pub) {
                        functions.push(func);
                    }
                }
                TokenKind::Struct => {
                    if let Some(st) = self.parse_struct_def(is_pub) {
                        structs.push(st);
                    }
                }
                TokenKind::Impl => {
                    let impl_methods = self.parse_impl_block();
                    functions.extend(impl_methods);
                }
                TokenKind::Static => {
                    if let Some(st) = self.parse_static_def(is_pub) {
                        statics.push(st);
                    }
                }
                TokenKind::Mod => {
                    if let Some((name, prog)) = self.parse_module(is_pub) {
                        modules.push((name, prog, is_pub));
                    }
                }
                TokenKind::Use => {
                    if let Some(stmt) = self.parse_use() {
                        if let Stmt::Use(path) = stmt {
                            uses.push(path);
                        }
                    }
                }

                _ => {
                    if let Some(end) = end_token {
                        if self.stream.is(end) {
                            break;
                        }
                    }

                    self.emit_error_at_current(ParseErrorKind::UnexpectedToken {
                        expected: TokenKind::Fn,
                        found: self.stream.current().kind,
                    });
                    self.advance();
                }
            }
        }

        Program {
            modules,
            structs,
            functions,
            statics,
            uses,
        }
    }

    fn optional(&mut self, kind: TokenKind) {
        if self.stream.is(kind) {
            self.advance();
        }
    }

    fn skip_newlines(&mut self) {
        self.optional(TokenKind::Newline);
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
        vec![]
    }

    fn parse_path(&mut self) -> Option<Vec<String>> {
        let mut path = Vec::new();

        if !self.stream.is(TokenKind::Ident) {
            return None;
        }

        path.push(self.stream.current_lit().to_string());
        self.advance();

        while self.stream.is(TokenKind::ColonColon) {
            if self.stream.is_peek(TokenKind::Lt) {
                break;
            }

            self.advance();

            if !self.stream.is(TokenKind::Ident) {
                self.emit_error_at_current(ParseErrorKind::Expected(
                    "Identifier after '::'".to_string(),
                ));
                return None;
            }

            path.push(self.stream.current_lit().to_string());
            self.advance();
        }

        Some(path)
    }
}
