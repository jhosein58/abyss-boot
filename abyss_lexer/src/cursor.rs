use core::str::Chars;

pub struct Cursor<'a> {
    len_remaining: usize,
    pub chars: Chars<'a>,
    prev: char,
}

pub(crate) const EOF_CHAR: char = '\0';

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Cursor<'a> {
        Cursor {
            len_remaining: input.len(),
            chars: input.chars(),
            prev: EOF_CHAR,
        }
    }

    pub fn as_str(&self) -> &'a str {
        self.chars.as_str()
    }

    pub fn peak(&self, n: usize) -> char {
        self.chars.clone().nth(n).unwrap_or(EOF_CHAR)
    }

    pub fn first(&self) -> char {
        self.peak(1)
    }

    pub fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }
}
