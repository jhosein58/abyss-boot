use crate::source_map::Span;

#[derive(Debug)]
pub enum Expr {
    Lit(Lit),
    Ident(Ident),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Grouped(GroupedExpr),
    Call(CallExpr),
    Let(LetExpr),
    Assign(AssignExpr),
    Return(ReturnExpr),
    Block(BlockExpr),
    If(IfExpr),
    While(WhileExpr),
    Err,
}

#[derive(Debug)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

#[derive(Debug)]
pub enum Type {
    Named(Ident),
    Pointer(Box<Type>),
}
#[derive(Debug)]
pub enum LitKind {
    Int { value: u128, suffix: Option<Type> },

    Float { value: String, suffix: Option<Type> },

    String(String),

    Char(char),

    Bool(bool),
}

#[derive(Debug)]
pub struct Lit {
    pub kind: LitKind,
    pub span: Span,
}

#[derive(Debug)]
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
}

#[derive(Debug)]
pub enum UnaryOp {
    Neg,    // -x
    Not,    // not x
    Deref,  // *x
    AddrOf, // &x
}

#[derive(Debug)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: BinaryOp,
    pub right: Box<Expr>,
    pub span: Span,
}

#[derive(Debug)]
pub struct UnaryExpr {
    pub operator: UnaryOp,
    pub operand: Box<Expr>,
    pub span: Span,
}

#[derive(Debug)]
pub struct GroupedExpr {
    pub expr: Box<Expr>,
    pub span: Span,
}

#[derive(Debug)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug)]
pub struct LetExpr {
    pub name: Ident,
    pub value: Option<Box<Expr>>,
    pub ty: Option<Type>,
    pub span: Span,
}

#[derive(Debug)]
pub struct AssignExpr {
    pub target: Box<Expr>,
    pub value: Box<Expr>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ReturnExpr {
    pub value: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug)]
pub struct BlockExpr {
    pub scope: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug)]
pub struct IfExpr {
    pub condition: Box<Expr>,
    pub then_branch: Box<Expr>,
    pub else_branch: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug)]
pub struct WhileExpr {
    pub condition: Box<Expr>,
    pub body: Box<Expr>,
    pub span: Span,
}

#[derive(Debug)]
pub struct FunctionParam {
    pub name: Ident,
    pub ty: Type,
}

#[derive(Debug)]
pub struct Function {
    pub name: Ident,
    pub params: Vec<FunctionParam>,
    pub return_type: Option<Type>,
    pub body: BlockExpr,
}

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Function>,
}
