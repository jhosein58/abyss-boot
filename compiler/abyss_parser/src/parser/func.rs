use std::{fs, path::PathBuf};

use abyss_lexer::token::TokenKind;

use crate::{
    ast::{FunctionBody, FunctionDef, Program, StaticDef, StructDef, Type},
    error::ParseErrorKind,
    parser::Parser,
};

impl<'a> Parser<'a> {
    fn synchronize_func(&mut self) {
        self.stream.advance();

        while !self.stream.is_at_end() {
            match self.stream.current().kind {
                TokenKind::Fn | TokenKind::Struct | TokenKind::Static | TokenKind::Pub => {
                    return;
                }
                _ => {}
            }

            self.stream.advance();
        }
    }
    fn consume_safely(&mut self, expected: TokenKind) -> Option<()> {
        if !self.stream.consume(expected) {
            self.emit_error_at_current(ParseErrorKind::UnexpectedToken {
                expected,
                found: self.stream.current().kind,
            });
            self.synchronize_func();
            return None;
        }
        Some(())
    }
    pub fn parse_function(&mut self, is_pub: bool) -> Option<FunctionDef> {
        self.consume_safely(TokenKind::Fn)?;

        let name = self.read_ident()?;

        let mut generics = Vec::new();
        if self.stream.is(TokenKind::Lt) {
            self.advance();
            while !self.stream.is(TokenKind::Gt) && !self.stream.is_at_end() {
                let gen_name = self.read_ident()?;
                generics.push(gen_name);
                if self.stream.is(TokenKind::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }
            self.consume_safely(TokenKind::Gt)?;
        }

        let params = self.parse_func_params()?;
        let return_type = self.parse_return_type();

        let body = if self.stream.is(TokenKind::Semi) || self.stream.is(TokenKind::Semi) {
            self.advance();
            FunctionBody::Extern
        } else {
            if let Some(stmts) = self.parse_block() {
                FunctionBody::UserDefined(stmts)
            } else {
                self.synchronize();
                return None;
            }
        };

        Some(FunctionDef {
            is_pub,
            name,
            generics,
            params,
            return_type,
            body,
        })
    }

    pub fn parse_struct_def(&mut self, is_pub: bool) -> Option<StructDef> {
        self.consume_safely(TokenKind::Struct)?;

        let name = self.read_ident()?;

        let mut generics = Vec::new();
        if self.stream.is(TokenKind::Lt) {
            self.advance();
            while !self.stream.is(TokenKind::Gt) && !self.stream.is_at_end() {
                let gen_name = self.read_ident()?;
                generics.push(gen_name);
                if self.stream.is(TokenKind::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }
            self.consume_safely(TokenKind::Gt)?;
        }

        self.consume_safely(TokenKind::OBrace)?;

        let mut fields = Vec::new();

        while !self.stream.is(TokenKind::CBrace) && !self.stream.is_at_end() {
            self.skip_newlines();
            if self.stream.is(TokenKind::CBrace) {
                break;
            }

            let field_name = self.read_ident()?;
            self.consume_safely(TokenKind::Colon)?;
            let field_type = self.parse_type()?;

            fields.push((field_name, field_type));

            if self.stream.is(TokenKind::Comma) {
                self.advance();
            }
            self.skip_newlines();
        }

        self.consume_safely(TokenKind::CBrace)?;

        Some(StructDef {
            is_pub,
            name,
            fields,
            generics,
        })
    }

    pub fn parse_static_def(&mut self, is_pub: bool) -> Option<StaticDef> {
        self.consume_safely(TokenKind::Static)?;

        let name = self.read_ident()?;

        self.consume_safely(TokenKind::Colon)?;
        let ty = self.parse_type()?;

        let has_eq = if self.stream.is(TokenKind::Assign) {
            self.advance();
            true
        } else if self.stream.is(TokenKind::Assign) {
            self.advance();
            true
        } else {
            false
        };

        let init_value = if has_eq {
            Some(self.parse_expr()?)
        } else {
            None
        };

        if !self.stream.consume(TokenKind::Semi) && !self.stream.consume(TokenKind::Semi) {
            self.emit_error_at_current(ParseErrorKind::UnexpectedToken {
                expected: TokenKind::Semi,
                found: self.stream.current().kind,
            });

            self.synchronize_func();
            return None;
        }

        if let Some(val) = init_value {
            Some(StaticDef {
                generics: vec![],
                is_pub,
                name,
                ty,
                value: val,
            })
        } else {
            self.emit_error_at_current(ParseErrorKind::Message(
                "Static variables must have an initial value".to_string(),
            ));
            None
        }
    }

    fn read_ident(&mut self) -> Option<String> {
        if self.stream.is(TokenKind::Ident) {
            let ident = self.stream.current_lit().to_string();
            self.advance();
            Some(ident)
        } else {
            self.emit_error_at_current(ParseErrorKind::Expected("ident".to_string()));
            self.synchronize_func();
            None
        }
    }

    fn parse_func_params(&mut self) -> Option<Vec<(String, Type)>> {
        let mut params = Vec::new();
        if !self.stream.is(TokenKind::OParen) {
            return Some(params);
        }
        self.advance();
        if self.stream.is(TokenKind::CParen) {
            self.advance();
            return Some(params);
        }

        loop {
            let name = self.read_ident()?;
            self.consume_safely(TokenKind::Colon)?;
            let ty = self.parse_type()?;
            params.push((name, ty));
            if self.stream.is(TokenKind::Comma) {
                self.advance();
                continue;
            }
            break;
        }
        self.consume_safely(TokenKind::CParen)?;
        Some(params)
    }

    fn parse_return_type(&mut self) -> Type {
        if self.stream.is(TokenKind::Colon) {
            self.advance();
            if let Some(ty) = self.parse_type() {
                return ty;
            }
        }
        Type::Void
    }

    pub fn parse_impl_block(&mut self) -> Vec<FunctionDef> {
        if !self.consume_safely(TokenKind::Impl).is_some() {
            return Vec::new();
        }

        let struct_name = match self.read_ident() {
            Some(name) => name,
            None => return Vec::new(),
        };

        if !self.consume_safely(TokenKind::OBrace).is_some() {
            return Vec::new();
        }

        let mut methods = Vec::new();

        while !self.stream.is(TokenKind::CBrace) && !self.stream.is_at_end() {
            self.skip_newlines();

            if self.stream.is(TokenKind::CBrace) {
                break;
            }

            let is_pub = if self.stream.is(TokenKind::Pub) {
                self.advance();
                true
            } else {
                false
            };

            if self.stream.is(TokenKind::Fn) {
                if let Some(mut func) = self.parse_function(is_pub) {
                    let old_name = func.name.clone();
                    let new_name = format!("{}__{}", struct_name, old_name);
                    func.name = new_name;

                    methods.push(func);
                }
            } else {
                self.emit_error_at_current(ParseErrorKind::UnexpectedToken {
                    expected: TokenKind::Fn,
                    found: self.stream.current().kind,
                });
                self.synchronize_func();
            }

            self.skip_newlines();
        }

        self.consume_safely(TokenKind::CBrace);

        methods
    }

    fn parse_mod_path(&mut self) -> Option<Vec<String>> {
        let mut path = Vec::new();
        path.push(self.read_ident()?);

        while self.stream.is(TokenKind::Dot) {
            self.advance();
            path.push(self.read_ident()?);
        }
        Some(path)
    }
    pub fn parse_module(&mut self, _: bool) -> Option<(String, Program)> {
        self.consume_safely(TokenKind::Mod)?;

        let path_segments = self.parse_mod_path()?;

        let final_module_name = if self.stream.is(TokenKind::As) {
            self.advance();
            self.read_ident()?
        } else {
            path_segments.last().cloned().unwrap()
        };

        if self.stream.is(TokenKind::OBrace) {
            if path_segments.len() > 1 {
                self.emit_error_at_current(ParseErrorKind::Message(
                    "Inline modules cannot have paths (use simple identifier)".to_string(),
                ));
            }

            self.advance();
            let program = self.parse_definitions(Some(TokenKind::CBrace));
            self.consume_safely(TokenKind::CBrace)?;
            return Some((final_module_name, program));
        } else if self.stream.is(TokenKind::Semi) {
            self.consume_safely(TokenKind::Semi)?;

            let mut relative_path = PathBuf::new();
            for seg in &path_segments {
                relative_path.push(seg);
            }

            let direct_file_path = self.root_dir.join(&relative_path).with_extension("a");

            let dir_path = self.root_dir.join(&relative_path);

            if direct_file_path.exists() && direct_file_path.is_file() {
                return self.load_single_file_module(&final_module_name, &direct_file_path);
            }

            if dir_path.exists() && dir_path.is_dir() {
                let mod_file_path = dir_path.join("mod.a");

                if mod_file_path.exists() {
                    return self.load_single_file_module(&final_module_name, &mod_file_path);
                } else {
                    return self.load_directory_modules(&final_module_name, &dir_path);
                }
            }

            self.emit_error_at_current(ParseErrorKind::Message(format!(
                "Could not resolve module '{}'. Looked for file at {:?} or directory at {:?}",
                final_module_name, direct_file_path, dir_path
            )));
            return None;
        } else {
            self.emit_error_at_current(ParseErrorKind::UnexpectedToken {
                expected: TokenKind::OBrace,
                found: self.stream.current().kind,
            });
            None
        }
    }

    fn load_single_file_module(
        &mut self,
        mod_name: &str,
        file_path: &PathBuf,
    ) -> Option<(String, Program)> {
        match fs::read_to_string(file_path) {
            Ok(content) => {
                let path_str = file_path.to_string_lossy().to_string();
                let mut sub_parser = Parser::new(&content, &path_str);
                let program = sub_parser.parse_program();

                if sub_parser.has_errors() {
                    self.emit_error_at_current(ParseErrorKind::Message(format!(
                        "Module '{}' (in {:?}) has parsing errors.",
                        mod_name, file_path
                    )));
                }

                Some((mod_name.to_string(), program))
            }
            Err(e) => {
                self.emit_error_at_current(ParseErrorKind::Message(format!(
                    "IO Error reading module '{}': {}",
                    mod_name, e
                )));
                None
            }
        }
    }

    fn load_directory_modules(
        &mut self,
        mod_name: &str,
        dir_path: &PathBuf,
    ) -> Option<(String, Program)> {
        let mut folder_program = Program {
            functions: Vec::new(),
            modules: Vec::new(),
            statics: Vec::new(),
            structs: Vec::new(),
        };
        match fs::read_dir(dir_path) {
            Ok(entries) => {
                for entry in entries {
                    if let Ok(entry) = entry {
                        let path = entry.path();

                        if path.is_file() && path.extension().and_then(|s| s.to_str()) == Some("a")
                        {
                            if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
                                let sub_mod_name = stem.to_string();

                                if let Some((_, sub_prog)) =
                                    self.load_single_file_module(&sub_mod_name, &path)
                                {
                                    folder_program.modules.push((sub_mod_name, sub_prog, true));
                                }
                            }
                        }
                    }
                }
                Some((mod_name.to_string(), folder_program))
            }
            Err(e) => {
                self.emit_error_at_current(ParseErrorKind::Message(format!(
                    "IO Error reading directory '{}': {}",
                    mod_name, e
                )));
                None
            }
        }
    }
}
