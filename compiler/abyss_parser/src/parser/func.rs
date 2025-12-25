use std::{fs, path::PathBuf};

use abyss_lexer::token::TokenKind;

use crate::{
    ast::{FunctionBody, FunctionDef, Program, StaticDef, Stmt, StructDef, Type},
    error::ParseErrorKind,
    parser::Parser,
};

impl<'a> Parser<'a> {
    fn synchronize_func(&mut self) {
        self.stream.advance();
        while !self.stream.is_at_end() {
            match self.stream.current().kind {
                TokenKind::Fn | TokenKind::Struct | TokenKind::Static | TokenKind::Pub => return,
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

    fn parse_generic_params(&mut self) -> Option<Vec<String>> {
        if self.stream.is(TokenKind::Lt) {
            self.advance();
            let mut generics = Vec::new();

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
            Some(generics)
        } else {
            Some(Vec::new())
        }
    }

    pub fn parse_function(&mut self, is_pub: bool) -> Option<FunctionDef> {
        self.consume_safely(TokenKind::Fn)?;

        let name = self.read_ident()?;

        let generics = self.parse_generic_params()?;

        let (params, is_variadic) = self.parse_func_params()?;

        let return_type = self.parse_return_type();

        let body = if self.stream.is(TokenKind::Semi) {
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
            is_variadic,
        })
    }
    pub fn parse_struct_def(&mut self, is_pub: bool) -> Option<StructDef> {
        self.consume_safely(TokenKind::Struct)?;

        let name = self.read_ident()?;

        let generics = self.parse_generic_params()?;

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
            generics,
            fields,
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
        } else {
            false
        };

        let init_value = if has_eq {
            Some(self.parse_expr()?)
        } else {
            None
        };

        if !self.stream.consume(TokenKind::Semi) {
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

    fn consume_generics_usage(&mut self) {
        if self.stream.consume(TokenKind::Lt) {
            while !self.stream.is(TokenKind::Gt) && !self.stream.is_at_end() {
                self.advance();
            }
            self.stream.consume(TokenKind::Gt);
        }
    }

    fn parse_func_params(&mut self) -> Option<(Vec<(String, Type)>, bool)> {
        let mut params = Vec::new();
        let mut is_variadic = false;

        if !self.stream.is(TokenKind::OParen) {
            return Some((params, false));
        }
        self.advance();

        if self.stream.is(TokenKind::CParen) {
            self.advance();
            return Some((params, false));
        }

        loop {
            if self.stream.is(TokenKind::DotDot) {
                self.advance();
                is_variadic = true;
                break;
            }

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
        Some((params, is_variadic))
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

        let impl_generics = self.parse_generic_params().unwrap_or_default();

        let struct_name = match self.read_ident() {
            Some(name) => name,
            None => return Vec::new(),
        };

        if self.stream.is(TokenKind::Lt) {
            self.consume_generics_usage();
        }

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

                    if !impl_generics.is_empty() {
                        let mut combined_generics = impl_generics.clone();
                        combined_generics.extend(func.generics);
                        func.generics = combined_generics;
                    }

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

    pub fn parse_use(&mut self) -> Option<Stmt> {
        self.consume_safely(TokenKind::Use)?;

        let mut path = Vec::new();
        path.push(self.read_ident()?);

        while self.stream.is(TokenKind::ColonColon) {
            self.advance();
            path.push(self.read_ident()?);
        }

        self.consume_safely(TokenKind::Semi)?;
        Some(Stmt::Use(path))
    }

    fn parse_mod_path(&mut self) -> Option<Vec<String>> {
        let mut path = Vec::new();
        path.push(self.read_ident()?);

        while self.stream.is(TokenKind::ColonColon) {
            self.advance();
            path.push(self.read_ident()?);
        }
        Some(path)
    }
    pub fn parse_module(&mut self, _: bool) -> Option<(String, Program)> {
        self.consume_safely(TokenKind::Mod)?;

        let path_segments = self.parse_mod_path()?;
        let mod_name = path_segments.last().cloned().unwrap();

        if self.stream.is(TokenKind::OBrace) {
            self.advance();
            let program = self.parse_definitions(Some(TokenKind::CBrace));
            self.consume_safely(TokenKind::CBrace)?;
            return Some((mod_name, program));
        }

        self.consume_safely(TokenKind::Semi)?;

        let (file_path, is_dir) = self.resolve_module_path(&path_segments)?;

        if self.loaded_paths.contains(&file_path) {
            self.emit_error_at_current(ParseErrorKind::Message(format!(
                   "Module '{}' is already defined via 'mod'. To use it here, use 'use crate::path::to::{}';",
                   mod_name, mod_name
               )));
            return None;
        }

        self.loaded_paths.insert(file_path.clone());

        if is_dir {
            self.load_directory_modules(&mod_name, &file_path)
        } else {
            self.load_single_file_module(&mod_name, &file_path)
        }
    }

    fn resolve_module_path(&mut self, path_segments: &[String]) -> Option<(PathBuf, bool)> {
        let mut relative_path = PathBuf::new();
        for seg in path_segments {
            relative_path.push(seg);
        }

        let direct_file = self.root_dir.join(&relative_path).with_extension("a");
        if direct_file.exists() {
            return Some((direct_file, false));
        }

        let dir_path = self.root_dir.join(&relative_path);
        if dir_path.exists() && dir_path.is_dir() {
            return Some((dir_path, true));
        }

        self.emit_error_at_current(ParseErrorKind::Message(format!(
            "Module not found: {:?}",
            relative_path
        )));
        None
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
            uses: Vec::new(),
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
