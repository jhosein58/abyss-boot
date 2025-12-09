use abyss_parser::ast::{BinaryOp, UnaryOp};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Int,   // i64
    Float, // f64
    Bool,
    Void,
}

pub trait Target {
    type Value: Copy + Clone;
    type Block: Copy + Clone;

    fn start_program(&mut self);
    fn end_program(&mut self);

    fn predefine_function(&mut self, name: &str, params: &[(String, Type)], return_type: Type);

    fn start_function(&mut self, name: &str);

    fn add_function_param(&mut self, name: &str);

    fn start_function_body(&mut self);

    fn end_function(&mut self);

    fn declare_variable(&mut self, name: &str, ty: Type);

    fn define_variable(&mut self, name: &str, value: Self::Value);

    fn assign_variable(&mut self, name: &str, value: Self::Value);

    fn create_block(&mut self) -> Self::Block;
    fn seal_block(&mut self, block: Self::Block);
    fn switch_to_block(&mut self, block: Self::Block);

    fn jump(&mut self, target_block: Self::Block);

    fn branch(&mut self, condition: Self::Value, then_block: Self::Block, else_block: Self::Block);

    fn return_value(&mut self, value: Self::Value);
    fn return_void(&mut self);

    fn translate_lit_int(&mut self, value: i64) -> Self::Value;
    fn translate_lit_float(&mut self, value: f64) -> Self::Value;
    fn translate_lit_bool(&mut self, value: bool) -> Self::Value;
    fn translate_ident(&mut self, name: &str) -> Self::Value;

    fn translate_binary_op(
        &mut self,
        op: BinaryOp,
        lhs: Self::Value,
        rhs: Self::Value,
    ) -> Self::Value;

    fn translate_unary_op(&mut self, op: UnaryOp, value: Self::Value) -> Self::Value;

    fn call_function(&mut self, name: &str, args: Vec<Self::Value>) -> Self::Value;
}
