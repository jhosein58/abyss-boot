use abyss_analyzer::lir::{LirLiteral, LirType};
use abyss_parser::ast::{BinaryOp, UnaryOp};

pub trait Target {
    // ========================================================================
    // 1. Structure & Globals
    // ========================================================================

    fn start_program(&mut self);

    fn end_program(&mut self);

    fn define_struct(&mut self, name: &str, fields: &[(String, LirType)]);

    fn define_enum(&mut self, name: &str, variants: &[(String, LirType)]);

    fn define_global(&mut self, name: &str, ty: &LirType, has_init: bool);

    // ========================================================================
    // 2. Function Definitions
    // ========================================================================

    fn declare_extern_function(
        &mut self,
        name: &str,
        params: &[(String, LirType)],
        return_type: &LirType,
    );

    fn declare_function_proto(
        &mut self,
        name: &str,
        params: &[(String, LirType)],
        return_type: &LirType,
    );

    fn begin_function(&mut self, name: &str, params: &[(String, LirType)], return_type: &LirType);

    fn end_function(&mut self);

    // ========================================================================
    // 3. Statements
    // ========================================================================
    //
    fn stmt_var_decl(&mut self, name: &str, ty: &LirType, has_init: bool);

    fn stmt_var_init_end(&mut self);

    fn expr_array_init_start(&mut self, ty_hint: Option<&LirType>);
    fn expr_array_init_sep(&mut self);
    fn expr_array_init_end(&mut self);

    fn stmt_assign_start(&mut self, lhs_is_ptr: bool);
    fn stmt_assign_end(&mut self);

    fn stmt_return_start(&mut self); // return ...
    fn stmt_return_end(&mut self); // ;

    fn stmt_break(&mut self);
    fn stmt_continue(&mut self);
    fn stmt_expr_end(&mut self);

    // ========================================================================
    // 4. Control Flow Blocks
    // ========================================================================

    fn begin_block(&mut self);
    fn end_block(&mut self);

    // IF
    fn begin_if(&mut self); // if (
    fn begin_if_body(&mut self); // ) {
    fn begin_else(&mut self); // } else {
    fn end_if(&mut self); // }

    // WHILE
    fn begin_while(&mut self); // while (
    fn begin_while_body(&mut self); // ) {
    fn end_while(&mut self); // }

    // SWITCH
    fn begin_switch(&mut self); // switch (
    fn begin_switch_body(&mut self); // ) {
    fn begin_case(&mut self, lit: &LirLiteral); // case 10:
    fn begin_default(&mut self); // default:
    fn end_case(&mut self); // break; (optional based on logic)
    fn end_switch(&mut self); // }

    // ========================================================================
    // 5. Expressions
    // ========================================================================

    fn expr_lit(&mut self, lit: &LirLiteral);
    fn expr_ident(&mut self, name: &str);

    fn expr_binary_start(&mut self, op: BinaryOp);
    fn expr_binary_mid(&mut self, op: BinaryOp);
    fn expr_binary_end(&mut self);

    fn expr_unary_start(&mut self, op: UnaryOp);
    fn expr_unary_end(&mut self);

    // Call: func(arg1, arg2)
    fn expr_call_start(&mut self, name: &str);
    fn expr_call_arg_sep(&mut self);
    fn expr_call_end(&mut self);

    // Member Access: obj.field or ptr->field
    fn expr_member(&mut self, field: &str, is_pointer: bool);

    fn expr_sizeof(&mut self, ty: &LirType);

    // Index: arr[idx]
    fn expr_index_start(&mut self);
    fn expr_index_end(&mut self);

    // Cast: (type)val
    fn expr_cast_start(&mut self, target_ty: &LirType);
    fn expr_cast_end(&mut self);

    // Pointer ops
    fn expr_deref_start(&mut self); // *
    fn expr_deref_end(&mut self);
    fn expr_addrof_start(&mut self); // &
    fn expr_addrof_end(&mut self);

    fn expr_ternary_mid1(&mut self);
    fn expr_ternary_mid2(&mut self);
}
