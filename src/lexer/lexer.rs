use crate::lexer::token::{Token, TokenKind, lookup_ident};

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    ch: char,
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut l = Self {
            input: input.chars().collect(),
            position: 0,
            read_position: 0,
            ch: '\0',
            line: 1,
            column: 0,
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        if self.ch == '\n' {
            self.line += 1;
            self.column = 0;
        } else {
            self.column += 1;
        }

        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input[self.read_position]
        }
    }

    fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\r' {
            self.read_char();
        }
    }
    fn skip_comment(&mut self) {
        while self.ch != '\n' && self.ch != '\0' {
            self.read_char();
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let start_line = self.line;
        let start_col = self.column;

        let token = match self.ch {
            '\n' => {
                let t = Token::new(TokenKind::Newline, "\\n".to_string(), start_line, start_col);
                self.read_char();

                loop {
                    while self.ch == '\n' || self.ch == '\r' || self.ch == ' ' || self.ch == '\t' {
                        self.read_char();
                    }

                    if self.ch == '-' && self.peek_char() == '-' {
                        self.skip_comment();
                    } else {
                        break;
                    }
                }
                return t;
            }

            '=' => Token::new(
                TokenKind::Assign,
                self.ch.to_string(),
                start_line,
                start_col,
            ),
            '+' => Token::new(TokenKind::Plus, self.ch.to_string(), start_line, start_col),
            '-' => {
                if self.peek_char() == '-' {
                    self.skip_comment();
                    return self.next_token();
                } else {
                    Token::new(TokenKind::Minus, self.ch.to_string(), start_line, start_col)
                }
            }
            '*' => Token::new(TokenKind::Star, self.ch.to_string(), start_line, start_col),
            '/' => Token::new(TokenKind::Slash, self.ch.to_string(), start_line, start_col),
            ',' => Token::new(TokenKind::Comma, self.ch.to_string(), start_line, start_col),
            ':' => Token::new(TokenKind::Colon, self.ch.to_string(), start_line, start_col),
            '.' => Token::new(TokenKind::Dot, self.ch.to_string(), start_line, start_col),
            '(' => Token::new(
                TokenKind::OParen,
                self.ch.to_string(),
                start_line,
                start_col,
            ),
            ')' => Token::new(
                TokenKind::CParen,
                self.ch.to_string(),
                start_line,
                start_col,
            ),
            '{' => Token::new(
                TokenKind::OBrace,
                self.ch.to_string(),
                start_line,
                start_col,
            ),
            '}' => Token::new(
                TokenKind::CBrace,
                self.ch.to_string(),
                start_line,
                start_col,
            ),

            '"' => {
                let literal = self.read_string();
                return Token::new(TokenKind::String, literal, start_line, start_col);
            }

            '\0' => Token::new(TokenKind::EOF, "".to_string(), start_line, start_col),

            _ => {
                if is_letter(self.ch) {
                    let literal = self.read_identifier();
                    let kind = lookup_ident(&literal);
                    return Token::new(kind, literal, start_line, start_col);
                } else if is_digit(self.ch) {
                    let literal = self.read_number();
                    return Token::new(TokenKind::Number, literal, start_line, start_col);
                } else {
                    Token::new(
                        TokenKind::Illegal,
                        self.ch.to_string(),
                        start_line,
                        start_col,
                    )
                }
            }
        };

        self.read_char();
        token
    }

    fn read_identifier(&mut self) -> String {
        let pos = self.position;
        while is_letter(self.ch) || is_digit(self.ch) {
            self.read_char();
        }
        self.input[pos..self.position].iter().collect()
    }

    fn read_number(&mut self) -> String {
        let pos = self.position;
        let mut has_dot = false;

        while is_digit(self.ch) || (self.ch == '.' && !has_dot) {
            if self.ch == '.' {
                if !is_digit(self.peek_char()) {
                    break;
                }
                has_dot = true;
            }
            self.read_char();
        }
        self.input[pos..self.position].iter().collect()
    }

    fn read_string(&mut self) -> String {
        let pos = self.position + 1;
        loop {
            self.read_char();
            if self.ch == '"' || self.ch == '\0' {
                break;
            }
        }
        let res = self.input[pos..self.position].iter().collect();
        self.read_char();
        res
    }
}

fn is_letter(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
}

fn is_digit(ch: char) -> bool {
    ch.is_ascii_digit()
}
