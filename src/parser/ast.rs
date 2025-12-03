#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    I8,
    I16,
    I32,
    I64,

    U8,
    U16,
    U32,
    U64,

    F32,
    F64,

    Bool,
    Void,

    Pointer(Box<Type>),

    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq)]
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
    And, // &&
    Or,  // ||
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    Neg,    // -x
    Not,    // !x
    Deref,  // *x
    AddrOf, // &x
}

#[derive(Debug, PartialEq, Clone)]
pub struct Parameter {
    pub name: String,
    pub kind: String,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let {
        name: String,
        ty: Option<Type>,
        value: Option<Expression>,
    },

    Assign {
        target: Expression,
        value: Expression,
    },

    Return(Option<Expression>),

    Expression(Expression),

    Block(Vec<Statement>),

    If {
        condition: Expression,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },

    While {
        condition: Expression,
        body: Box<Statement>,
    },
}

#[derive(Debug, Clone)]
pub enum Expression {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    StringLiteral(String),

    Identifier(String),

    Binary {
        left: Box<Expression>,
        operator: BinaryOp,
        right: Box<Expression>,
    },

    Unary {
        operator: UnaryOp,
        operand: Box<Expression>,
    },

    Grouped(Box<Expression>),

    Call {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
}

#[derive(Debug, Clone)]
pub struct FunctionParam {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<FunctionParam>,
    pub return_type: Type,
    pub body: Statement,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<Function>,
}
