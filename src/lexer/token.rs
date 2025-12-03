#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    // --- Identifiers & Literals ---
    Ident,
    Number,
    String,

    // --- Keywords ---
    Let, // let
    Fn,  // fn
    Ret, // ret

    // --- Operators & Delimiters ---
    Assign, // =

    Plus,  // +
    Minus, // -
    Star,  // *
    Slash, // /

    Comma, // ,
    Colon, // :
    Dot,   // .

    OParen, // (
    CParen, // )
    OBrace, // {
    CBrace, // }

    // Delimiters
    Newline,

    // --- Special ---
    Illegal,
    EOF,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub literal: String,
    pub line: usize,
    pub column: usize,
}

impl Token {
    pub fn new(kind: TokenKind, literal: String, line: usize, column: usize) -> Self {
        Self {
            kind,
            literal,
            line,
            column,
        }
    }
}

pub fn lookup_ident(ident: &str) -> TokenKind {
    match ident {
        "let" => TokenKind::Let,
        "fn" => TokenKind::Fn,
        "ret" => TokenKind::Ret,
        _ => TokenKind::Ident,
    }
}
