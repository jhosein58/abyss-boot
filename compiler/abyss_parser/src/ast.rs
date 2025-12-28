pub type Path = Vec<String>;

#[derive(Debug, Clone)]
pub enum Stmt {
    Mod(Path, Option<Box<Stmt>>),
    Use(Path),
    Let(String, Option<Type>, Option<Expr>),
    Const(String, Option<Type>, Option<Expr>),
    FunctionDef(Box<FunctionDef>),
    StructDef(Box<StructDef>),
    UnionDef(Box<UnionDef>),
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
    Is(Box<Expr>, Type),
    Member(Box<Expr>, String),
    StructInit(Path, Vec<(String, Expr)>, Vec<Type>),
    UnionInit(Path, Vec<(String, Expr)>),
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
    Union(Vec<Type>),
}

impl Type {
    pub fn get_name(&self) -> String {
        match self {
            Type::U8 => "u8".to_string(),
            Type::U16 => "u16".to_string(),
            Type::U32 => "u32".to_string(),
            Type::U64 => "u64".to_string(),
            Type::Usize => "usize".to_string(),
            Type::I8 => "i8".to_string(),
            Type::I16 => "i16".to_string(),
            Type::I32 => "i32".to_string(),
            Type::I64 => "i64".to_string(),
            Type::Isize => "isize".to_string(),
            Type::F32 => "f32".to_string(),
            Type::F64 => "f64".to_string(),
            Type::Char => "char".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Void => "void".to_string(),
            Type::Pointer(ty) => format!("ptr_{}", ty.get_name()),
            Type::Const(ty) => format!("const_{}", ty.get_name()),
            Type::Array(ty, size) => format!("Arr_{}_{}", ty.get_name(), size),
            Type::Struct(path, _) => format!("struct_{}", path.join("_")),
            Type::Generic(name) => name.clone(),

            _ => panic!("Type has no Name"),
        }
    }
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
    pub external_name: Option<String>,
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
pub struct UnionDef {
    pub is_pub: bool,
    pub name: String,
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
    pub unions: Vec<UnionDef>,
    pub functions: Vec<FunctionDef>,
    pub statics: Vec<StaticDef>,
    pub uses: Vec<Path>,
}
