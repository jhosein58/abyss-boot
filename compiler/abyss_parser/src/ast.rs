pub type Path = Vec<String>;

#[derive(Debug, Clone)]
pub enum Stmt {
    Mod(Path, Option<Box<Stmt>>),
    Use(Path),
    Let(String, Option<Type>, Option<Expr>),
    Const(String, Option<Type>, Option<Expr>),
    FunctionDef(Box<FunctionDef>),
    StructDef(Box<StructDef>),
    Assign(Expr, Expr),
    Ret(Expr),
    Break,    // out
    Continue, // next
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Lit(Lit),
    Ident(Path),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>, Vec<Type>), // (callee)(args, generics)
    Index(Box<Expr>, Box<Expr>),
    Deref(Box<Expr>),
    AddrOf(Box<Expr>),
    Cast(Box<Expr>, Type),
    Member(Box<Expr>, String),
    StructInit(Path, Vec<(String, Expr)>, Vec<Type>),
    MethodCall(Box<Expr>, String, Vec<Expr>, Vec<Type>),
    SizeOf(Type),
    Match(Box<Expr>, Vec<(Pattern, Expr)>),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    U8,
    U16,
    U32,
    U64,
    Usize,
    I8,
    I16,
    I32,
    I64,
    Isize,
    F32,
    F64,
    Char,
    Bool,
    Void,
    Pointer(Box<Type>),
    Const(Box<Type>),
    Array(Box<Type>, usize),
    Struct(Path, Vec<Type>),
    Generic(String),
    Function(Vec<Type>, Box<Type>, Vec<Type>), // Function(args, return_type, generics)
}

#[derive(Debug, Clone)]
pub enum Lit {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Null,
    Array(Vec<Expr>),
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum BinaryOp {
    Assign, // =
    Add,    // +
    Sub,    // -
    Mul,    // *
    Div,    // /
    Mod,    // %
    Eq,     // ==
    Neq,    // !=
    Lt,     // <
    Gt,     // >
    Lte,    // <=
    Gte,    // >=
    And,    // and
    Or,     // or

    BitAnd, // &
    BitOr,  // |
    BitXor, // ^
    Shl,    // <<
    Shr,    // >>
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Neg,    // -x
    Not,    // not x
    BitNot, // ~x
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Lit(Lit),
    Variant(Path, Vec<String>),
    Wildcard,
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub is_pub: bool,
    pub name: String,
    pub generics: Vec<String>,
    pub params: Vec<(String, Type)>,
    pub return_type: Type,
    pub body: FunctionBody,
    pub is_variadic: bool,
}

#[derive(Debug, Clone)]
pub enum FunctionBody {
    UserDefined(Vec<Stmt>),
    Extern,
}

#[derive(Debug, Clone)]
pub struct StructDef {
    pub is_pub: bool,
    pub name: String,
    pub generics: Vec<String>,
    pub fields: Vec<(String, Type)>,
}

#[derive(Debug, Clone)]
pub struct StaticDef {
    pub is_pub: bool,
    pub name: String,
    pub generics: Vec<String>,
    pub ty: Type,
    pub value: Expr,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub modules: Vec<(String, Program, bool)>,

    pub structs: Vec<StructDef>,
    pub functions: Vec<FunctionDef>,
    pub statics: Vec<StaticDef>,
    pub uses: Vec<Path>,
}
