use abyss_parser::ast::{BinaryOp, UnaryOp};

#[derive(Debug, Clone)]
pub enum LirStmt {
    Let(String, LirType, Option<LirExpr>),

    Assign(LirExpr, LirExpr),

    ExprStmt(LirExpr),

    Return(Option<LirExpr>),
    Break,
    Continue,

    Block(Vec<LirStmt>),

    If {
        cond: LirExpr,
        then_branch: Vec<LirStmt>,
        else_branch: Vec<LirStmt>,
    },

    While {
        cond: LirExpr,
        body: Vec<LirStmt>,
    },

    Switch {
        expr: LirExpr,
        cases: Vec<(LirLiteral, Vec<LirStmt>)>,
        default: Vec<LirStmt>,
    },
}

#[derive(Debug, Clone)]
pub enum LirExpr {
    Lit(LirLiteral),

    Ident(String),

    Binary(Box<LirExpr>, BinaryOp, Box<LirExpr>),
    Unary(UnaryOp, Box<LirExpr>),

    Call {
        func_name: String,
        args: Vec<LirExpr>,
    },

    CallPtr(Box<LirExpr>, Vec<LirExpr>),

    MemberAccess(Box<LirExpr>, String),

    MemberAccessPtr(Box<LirExpr>, String),

    Index(Box<LirExpr>, Box<LirExpr>),
    AddrOf(Box<LirExpr>),
    Deref(Box<LirExpr>),

    Cast(Box<LirExpr>, LirType),

    StructInit {
        struct_name: String,
        fields: Vec<(String, LirExpr)>,
    },

    EnumInit {
        enum_name: String,
        variant_tag: usize,
        payload: Option<Box<LirExpr>>,
    },

    Ternary(Box<LirExpr>, Box<LirExpr>, Box<LirExpr>),

    SizeOf(LirType),

    ArrayInit(Vec<LirExpr>),
}

#[derive(Debug, Clone)]
pub enum LirLiteral {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LirType {
    U8,
    I64,
    F64,
    Bool,
    Void,
    Pointer(Box<LirType>),
    Array(Box<LirType>, usize),
    Struct(String),
    Enum(String),
    FunctionPtr(Vec<LirType>, Box<LirType>),
}

#[derive(Debug, Clone)]
pub struct LirStructDef {
    pub name: String,
    pub fields: Vec<(String, LirType)>,
}

#[derive(Debug, Clone)]
pub struct LirEnumDef {
    pub name: String,
    pub variants: Vec<(String, LirType)>,
}

#[derive(Debug, Clone)]
pub struct LirGlobalVar {
    pub name: String,
    pub ty: LirType,
    pub init_value: Option<LirExpr>,
}

#[derive(Debug, Clone)]
pub struct LirFunctionDef {
    pub name: String,
    pub params: Vec<(String, LirType)>,
    pub return_type: LirType,
    pub body: Vec<LirStmt>,
    pub is_extern: bool,
}

#[derive(Debug, Clone, Default)]
pub struct LirProgram {
    pub structs: Vec<LirStructDef>,
    pub enums: Vec<LirEnumDef>,
    pub globals: Vec<LirGlobalVar>,
    pub functions: Vec<LirFunctionDef>,
}
