pub type Path = Vec<String>;

#[derive(Debug, Clone)]
pub enum Stmt {
    Import(Path),
    FromImport(Path, Vec<String>),
    Let(String, Option<Type>, Option<Expr>),
    Const(String, Option<Type>, Option<Expr>),
    FunctionDef(Box<FunctionDef>),
    StructDef(Box<StructDef>),
    EnumDef(Box<EnumDef>),
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
    EnumInit(Path, Vec<Expr>, Vec<Type>),
    MethodCall(Box<Expr>, String, Vec<Expr>, Vec<Type>),
    SizeOf(Type),
    Match(Box<Expr>, Vec<(Pattern, Expr)>),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    U8,
    I64,
    F64,
    Bool,
    Void,
    Pointer(Box<Type>),
    Array(Box<Type>, usize),
    Struct(Path, Vec<Type>),
    Enum(Path, Vec<Type>),
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

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %
    Eq,  // ==
    Neq, // !=
    Lt,  // <
    Gt,  // >
    Lte, // <=
    Gte, // >=
    And, // and
    Or,  // or

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
pub struct EnumDef {
    pub is_pub: bool,
    pub name: String,
    pub generics: Vec<String>,
    pub variants: Vec<(String, Vec<Type>)>,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub modules: Vec<(String, Program, bool)>,

    pub structs: Vec<StructDef>,
    pub functions: Vec<FunctionDef>,
    pub statics: Vec<StaticDef>,
    pub enums: Vec<EnumDef>,
}

pub fn get_debug_prog() -> Program {
    let deep_nested_func = FunctionDef {
        is_pub: false,
        name: "validator".to_string(),
        generics: vec![],
        params: vec![],
        return_type: Type::Bool,
        body: FunctionBody::UserDefined(vec![Stmt::Ret(Expr::Lit(Lit::Bool(true)))]),
    };

    let mid_nested_func = FunctionDef {
        is_pub: false,
        name: "processor".to_string(),
        generics: vec![],
        params: vec![("input".to_string(), Type::I64)],
        return_type: Type::I64,
        body: FunctionBody::UserDefined(vec![
            Stmt::FunctionDef(Box::new(deep_nested_func)),
            Stmt::Let(
                "is_valid".to_string(),
                Some(Type::Bool),
                Some(Expr::Call(
                    Box::new(Expr::Ident(vec!["validator".to_string()])),
                    vec![],
                    vec![],
                )),
            ),
            Stmt::If(
                Expr::Ident(vec!["is_valid".to_string()]),
                Box::new(Stmt::Ret(Expr::Binary(
                    Box::new(Expr::Ident(vec!["input".to_string()])),
                    BinaryOp::Mul,
                    Box::new(Expr::Lit(Lit::Int(2))),
                ))),
                Some(Box::new(Stmt::Ret(Expr::Lit(Lit::Int(0))))),
            ),
        ]),
    };

    let main_func = FunctionDef {
        is_pub: true,
        name: "main".to_string(),
        generics: vec![],
        params: vec![],
        return_type: Type::Void,
        body: FunctionBody::UserDefined(vec![
            Stmt::FunctionDef(Box::new(mid_nested_func)),
            Stmt::Expr(Expr::Call(
                Box::new(Expr::Ident(vec!["processor".to_string()])),
                vec![Expr::Lit(Lit::Int(42))],
                vec![],
            )),
        ]),
    };

    let mod_func = FunctionDef {
        is_pub: true,
        name: "calc".to_string(),
        generics: vec![],
        params: vec![],
        return_type: Type::I64,
        body: FunctionBody::UserDefined(vec![Stmt::Ret(Expr::Lit(Lit::Int(100)))]),
    };

    let sub_module = Program {
        modules: vec![],
        structs: vec![],
        functions: vec![mod_func],
        statics: vec![],
        enums: vec![],
    };

    let root_module = Program {
        modules: vec![("math".to_string(), sub_module, true)],
        structs: vec![],
        functions: vec![],
        statics: vec![],
        enums: vec![],
    };

    Program {
        modules: vec![("core".to_string(), root_module, false)],
        structs: vec![],
        functions: vec![main_func],
        statics: vec![],
        enums: vec![],
    }
}
