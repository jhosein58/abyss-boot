use core::fmt::{self, Display, Formatter, write};

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

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Comment => write!(f, "Comment"),
            TokenKind::Whitespace => write!(f, "Whitespace"),
            TokenKind::Newline => write!(f, "Newline"),
            TokenKind::Ident => write!(f, "Ident"),
            TokenKind::Literal(lit) => write!(f, "Literal({})", lit),
            TokenKind::Let => write!(f, "'let'"),
            TokenKind::Fn => write!(f, "'fn'"),
            TokenKind::Ret => write!(f, "'ret'"),
            TokenKind::If => write!(f, "'if'"),
            TokenKind::While => write!(f, "'while'"),
            TokenKind::As => write!(f, "'as'"),
            TokenKind::And => write!(f, "'and'"),
            TokenKind::Or => write!(f, "'or'"),
            TokenKind::Not => write!(f, "'not'"),
            TokenKind::Plus => write!(f, "'+'"),
            TokenKind::Minus => write!(f, "'-'"),
            TokenKind::Star => write!(f, "'*'"),
            TokenKind::Slash => write!(f, "'/'"),
            TokenKind::Percent => write!(f, "'%'"),
            TokenKind::Comma => write!(f, "','"),
            TokenKind::Colon => write!(f, "':'"),
            TokenKind::Dot => write!(f, "'.'"),
            TokenKind::OParen => write!(f, "'('"),
            TokenKind::CParen => write!(f, "')'"),
            TokenKind::OBrace => write!(f, "'{{'"),
            TokenKind::CBrace => write!(f, "'}}'"),
            TokenKind::OBracket => write!(f, "'['"),
            TokenKind::CBracket => write!(f, "']'"),
            TokenKind::Assign => write!(f, "'='"),
            TokenKind::EqEq => write!(f, "'=='"),
            TokenKind::BangEq => write!(f, "'!='"),
            TokenKind::Lt => write!(f, "'<'"),
            TokenKind::LtEq => write!(f, "'<='"),
            TokenKind::Gt => write!(f, "'>'"),
            TokenKind::GtEq => write!(f, "'>='"),
            TokenKind::Unknown => write!(f, "Unknown"),
            TokenKind::Eof => write!(f, "Eof"),
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

impl Display for LiteralKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LiteralKind::Int => write!(f, "int"),
            LiteralKind::Float => write!(f, "float"),
            LiteralKind::Str => write!(f, "string"),
            LiteralKind::Char => write!(f, "char"),
            LiteralKind::Bool => writeln!(f, "bool"),
        }
    }
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

    pub fn dummy() -> Self {
        Self::new(TokenKind::Unknown, 0)
    }
}
