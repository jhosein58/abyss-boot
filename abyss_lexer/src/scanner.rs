use crate::{
    cursor::Cursor,
    token::{RawToken, RawTokenKind},
};

pub struct Scanner<'a> {
    pub cursor: Cursor<'a>,
}

impl<'a> Scanner<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            cursor: Cursor::new(input),
        }
    }

    fn is_simple_whitespace(c: char) -> bool {
        matches!(c, ' ' | '\t')
    }

    fn is_newline(c: char) -> bool {
        matches!(c, '\n' | '\r')
    }

    fn is_ident_start(c: char) -> bool {
        c.is_ascii_alphabetic() || c == '_'
    }

    fn is_ident_continue(c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '_'
    }

    fn is_string_start(c: char) -> bool {
        c == '"'
    }

    fn is_digit(c: char) -> bool {
        c.is_ascii_digit()
    }

    pub fn next_raw(&mut self) -> RawToken {
        if self.cursor.is_eof() {
            return RawToken::new(RawTokenKind::Eof, 0);
        }

        let start_pos = self.cursor.len_consumed();
        let first_char = self.cursor.first();

        let kind = if Self::is_newline(first_char) {
            self.consume_newlines();
            RawTokenKind::Newline
        } else if Self::is_simple_whitespace(first_char) {
            self.consume_simple_whitespace();
            RawTokenKind::Whitespace
        } else if Self::is_ident_start(first_char) {
            self.scan_identifier();
            RawTokenKind::Ident
        } else if Self::is_digit(first_char) {
            self.scan_number()
        } else if Self::is_string_start(first_char) {
            self.scan_string()
        } else if first_char == '-' && self.cursor.second() == '-' {
            self.scan_comment();
            RawTokenKind::Comment
        } else {
            self.cursor.bump();
            RawTokenKind::Symbol
        };

        let end_pos = self.cursor.len_consumed();
        let len = end_pos - start_pos;

        RawToken::new(kind, len)
    }

    fn consume_newlines(&mut self) {
        self.cursor.eat_while(Self::is_newline);
    }

    fn consume_simple_whitespace(&mut self) {
        self.cursor.eat_while(Self::is_simple_whitespace);
    }

    fn scan_identifier(&mut self) {
        self.cursor.eat_while(Self::is_ident_continue);
    }

    fn scan_comment(&mut self) {
        self.cursor.bump();
        self.cursor.bump();
        self.cursor.eat_while(|c| c != '\n' && c != '\r');
    }

    fn scan_number(&mut self) -> RawTokenKind {
        self.cursor.eat_while(Self::is_digit);
        if self.cursor.first() == '.' && Self::is_digit(self.cursor.second()) {
            self.cursor.bump();
            self.cursor.eat_while(Self::is_digit);
            return RawTokenKind::Float;
        }
        RawTokenKind::Int
    }

    fn scan_string(&mut self) -> RawTokenKind {
        self.cursor.bump();
        self.cursor.eat_while(|c| c != '"');
        if !self.cursor.is_eof() {
            self.cursor.bump();
        }
        RawTokenKind::String
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = RawToken;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_raw();

        if let RawTokenKind::Eof = token.kind {
            None
        } else {
            Some(token)
        }
    }
}
