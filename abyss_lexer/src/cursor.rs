use core::str::Chars;

pub(crate) const EOF_CHAR: char = '\0';

pub struct Cursor<'a> {
    initial_len: usize,
    chars: Chars<'a>,
}

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Cursor<'a> {
        Cursor {
            initial_len: input.len(),
            chars: input.chars(),
        }
    }

    pub fn as_str(&self) -> &'a str {
        self.chars.as_str()
    }

    pub fn len_remaining(&self) -> usize {
        self.chars.as_str().len()
    }

    pub fn len_consumed(&self) -> usize {
        self.initial_len - self.len_remaining()
    }

    pub fn peek(&self, n: usize) -> char {
        self.chars.clone().nth(n).unwrap_or(EOF_CHAR)
    }

    pub fn first(&self) -> char {
        self.peek(0)
    }

    pub fn second(&self) -> char {
        self.peek(1)
    }

    pub fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    pub fn bump(&mut self) -> Option<char> {
        self.chars.next()
    }

    pub fn eat_if(&mut self, predicate: impl Fn(char) -> bool) -> bool {
        if !self.is_eof() && predicate(self.first()) {
            self.bump();
            true
        } else {
            false
        }
    }

    pub fn eat_while(&mut self, predicate: impl Fn(char) -> bool) {
        while !self.is_eof() && predicate(self.first()) {
            self.bump();
        }
    }
}
