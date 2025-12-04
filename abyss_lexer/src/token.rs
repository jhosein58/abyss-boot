#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RawTokenKind {
    Comment,
    Whitespace,
    Newline,

    Ident,

    Int,
    Float,
    String,

    Symbol,

    Eof,
}

#[derive(Debug, Clone, Copy)]
pub struct RawToken {
    pub kind: RawTokenKind,
    pub len: usize,
}

impl RawToken {
    pub fn new(kind: RawTokenKind, len: usize) -> Self {
        Self { kind, len }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    Comment,
    Whitespace,
    Newline,

    Ident,

    Literal(LiteralKind),

    // --- Keywords ---
    Let,   // let
    Fn,    // fn
    Ret,   // ret
    If,    // if
    While, // while
    As,    // as
    And,   // and
    Or,    // or
    Not,   // not

    Plus,     // +
    Minus,    // -
    Star,     // *
    Slash,    // /
    Percent,  // %
    Comma,    // ,
    Colon,    // :
    Dot,      // .
    OParen,   // (
    CParen,   // )
    OBrace,   // {
    CBrace,   // }
    OBracket, // [
    CBracket, // ]
    Assign,   // =
    EqEq,     // ==
    BangEq,   // !=
    Lt,       // <
    LtEq,     // <=
    Gt,       // >
    GtEq,     // >=

    Unknown,
    Eof,
}

impl TokenKind {
    pub fn lookup_ident(ident: &str) -> TokenKind {
        match ident {
            "let" => TokenKind::Let,
            "fn" => TokenKind::Fn,
            "ret" => TokenKind::Ret,
            "if" => TokenKind::If,
            "while" => TokenKind::While,
            "and" => TokenKind::And,
            "or" => TokenKind::Or,
            "not" => TokenKind::Not,
            "as" => TokenKind::As,
            _ => TokenKind::Ident,
        }
    }

    pub fn lookup_symbol(sym: &str) -> TokenKind {
        match sym {
            "+" => TokenKind::Plus,
            "-" => TokenKind::Minus,
            "*" => TokenKind::Star,
            "/" => TokenKind::Slash,
            "%" => TokenKind::Percent,
            "," => TokenKind::Comma,
            ":" => TokenKind::Colon,
            "." => TokenKind::Dot,
            "(" => TokenKind::OParen,
            ")" => TokenKind::CParen,
            "{" => TokenKind::OBrace,
            "}" => TokenKind::CBrace,
            "[" => TokenKind::OBracket,
            "]" => TokenKind::CBracket,
            "=" => TokenKind::Assign,
            "==" => TokenKind::EqEq,
            "!=" => TokenKind::BangEq,
            "<" => TokenKind::Lt,
            "<=" => TokenKind::LtEq,
            ">" => TokenKind::Gt,
            ">=" => TokenKind::GtEq,
            _ => TokenKind::Unknown,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum LiteralKind {
    Int,
    Float,
    Str,
    Char,
    Bool,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub len: usize,
}

impl Token {
    pub fn new(kind: TokenKind, len: usize) -> Self {
        Self { kind, len }
    }
}
