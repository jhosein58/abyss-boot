use crate::target::Target;
use abyss_analyzer::lir::{LirLiteral, LirType};
use abyss_parser::ast::{BinaryOp, UnaryOp};
pub struct CTarget {
    output: String,
    indent_level: usize,
    pending_newline: bool,
    in_variable_init: bool,
    init_state_stack: Vec<bool>,
}

impl CTarget {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            indent_level: 0,
            pending_newline: false,
            in_variable_init: false,
            init_state_stack: Vec::new(),
        }
    }

    fn push_indent(&mut self) {
        self.indent_level += 1;
    }

    fn pop_indent(&mut self) {
        if self.indent_level > 0 {
            self.indent_level -= 1;
        }
    }

    fn write(&mut self, text: &str) {
        if self.pending_newline {
            self.output.push('\n');
            self.output.push_str(&"    ".repeat(self.indent_level));
            self.pending_newline = false;
        }
        self.output.push_str(text);
    }

    fn set_newline_pending(&mut self) {
        self.pending_newline = true;
    }

    // --- Helper Methods ---

    fn type_to_c(&self, ty: &LirType) -> String {
        match ty {
            LirType::U8 => "unsigned char".to_string(),
            LirType::U16 => "unsigned short".to_string(),
            LirType::U32 => "unsigned int".to_string(),
            LirType::U64 => "unsigned long long".to_string(),
            LirType::Usize => "unsigned long long".to_string(),
            LirType::I8 => "signed char".to_string(),
            LirType::I16 => "short".to_string(),
            LirType::I32 => "int".to_string(),
            LirType::I64 => "long long".to_string(),
            LirType::Isize => "long long".to_string(),
            LirType::F32 => "float".to_string(),
            LirType::F64 => "double".to_string(),
            LirType::Char => "char".to_string(),
            LirType::Bool => "int".to_string(),
            LirType::Void => "void".to_string(),
            LirType::Pointer(inner) => format!("{}*", self.type_to_c(inner)),
            LirType::Const(inner) => format!("const {}", self.type_to_c(inner)),

            LirType::Array(inner, _) => format!("{}*", self.type_to_c(inner)),
            LirType::Struct(name) => {
                if name.starts_with("__UnionInner_") {
                    format!("union {}", name)
                } else {
                    format!("struct {}", name)
                }
            }
            LirType::Union(variants) => {
                let mut variant_names: Vec<String> =
                    variants.iter().map(|v| v.get_name()).collect();

                variant_names.sort();

                let id = variant_names.join("_");
                format!("struct __Union_{}", id)
            }

            LirType::FunctionPtr(args, ret) => {
                let args_str = args
                    .iter()
                    .map(|t| self.type_to_c(t))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{} (*)({})", self.type_to_c(ret), args_str)
            }
        }
    }

    fn literal_to_c(&self, lit: &LirLiteral) -> String {
        match lit {
            LirLiteral::Int(i) => format!("{}LL", i),
            LirLiteral::Float(f) => format!("{}", f),
            LirLiteral::Byte(b) => format!("{}", b),
            LirLiteral::Bool(b) => {
                if *b {
                    "1".to_string()
                } else {
                    "0".to_string()
                }
            }

            LirLiteral::Null => "((void*)0)".to_string(),
            LirLiteral::Array(_) => "NULL; // Array literal not supported".to_string(),
        }
    }
    fn binary_op_to_c(&self, op: BinaryOp) -> &'static str {
        match op {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
            BinaryOp::Eq => "==",
            BinaryOp::Neq => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::Gt => ">",
            BinaryOp::Lte => "<=",
            BinaryOp::Gte => ">=",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
            BinaryOp::BitAnd => "&",
            BinaryOp::BitOr => "|",
            BinaryOp::BitXor => "^",
            BinaryOp::Shl => "<<",
            BinaryOp::Shr => ">>",
            BinaryOp::Assign => "=",
        }
    }

    fn unary_op_to_c(&self, op: UnaryOp) -> &'static str {
        match op {
            UnaryOp::Neg => "-",
            UnaryOp::Not => "!",
            UnaryOp::BitNot => "~",
        }
    }

    pub fn get_code(&self) -> &str {
        &self.output
    }

    fn params_to_func_args(&self, params: &[(String, LirType)], is_variadic: bool) -> String {
        let mut args_str = if params.is_empty() {
            if is_variadic {
                "...".to_string()
            } else {
                "void".to_string()
            }
        } else {
            params
                .iter()
                .map(|(name, ty)| format!("{} {}", self.type_to_c(ty), name))
                .collect::<Vec<String>>()
                .join(", ")
        };

        if is_variadic && !params.is_empty() {
            args_str.push_str(", ...");
        }

        args_str
    }

    fn func_signature(
        &self,
        name: &str,
        params: &[(String, LirType)],
        return_type: &LirType,
        is_variadic: bool,
    ) -> String {
        format!(
            "{} {}({})",
            self.type_to_c(return_type),
            name,
            self.params_to_func_args(params, is_variadic)
        )
    }
}

impl Target for CTarget {
    fn emit(&mut self) -> String {
        self.get_code().to_string()
    }
    fn start_program(&mut self) {
        self.output.clear();
        self.output
            .push_str("// --- Generated by Abyss CTarget ---\n");
        self.output.push_str(include_str!("runtime/c_rt.c"));
        self.pending_newline = false;
    }

    fn end_program(&mut self) {
        self.indent_level = 0;
        self.output.push_str("\n// --- End of generated code ---\n");
        self.output.push_str("\nint main(void) {\n");
        self.indent_level = 1;
        self.pending_newline = true;
        self.write("app_main();");
        self.set_newline_pending();
        self.write("return 0;");
        self.indent_level = 0;
        self.output.push_str("\n}");
    }

    fn define_struct(&mut self, name: &str, fields: &[(String, LirType)]) {
        self.write(&format!("struct {} {{", name));
        self.push_indent();
        self.set_newline_pending();
        for (field_name, field_type) in fields {
            let decl = match field_type {
                LirType::Array(inner, size) => {
                    format!("{} {}[{}]", self.type_to_c(inner), field_name, size)
                }
                _ => format!("{} {}", self.type_to_c(field_type), field_name),
            };
            self.write(&format!("{};", decl));
            self.set_newline_pending();
        }
        self.pop_indent();
        self.write("};");
        self.set_newline_pending();
        self.write("");
        self.set_newline_pending();
    }

    fn define_union(&mut self, name: &str, variants: &[(String, LirType)]) {
        self.write(&format!("union {} {{", name));
        self.push_indent();
        self.set_newline_pending();
        for (field_name, field_type) in variants {
            let decl = match field_type {
                LirType::Array(inner, size) => {
                    format!("{} {}[{}]", self.type_to_c(inner), field_name, size)
                }
                _ => format!("{} {}", self.type_to_c(field_type), field_name),
            };
            self.write(&format!("{};", decl));
            self.set_newline_pending();
        }
        self.pop_indent();
        self.write("};");
        self.set_newline_pending();
        self.write("");
        self.set_newline_pending();
    }

    fn expr_union_init_start(&mut self, union_name: &str) {
        self.write(&format!("(union {}){{ ", union_name));
    }

    fn expr_union_init_field_start(&mut self, field_name: &str) {
        self.write(&format!(".{} = ", field_name));
    }

    fn expr_union_init_end(&mut self) {
        self.write(" }");
    }

    fn define_global_start(&mut self, name: &str, ty: &LirType, is_const: bool) {
        if is_const {
            self.write("const ");
        }

        let decl = match ty {
            LirType::Array(inner, size) => format!("{} {}[{}]", self.type_to_c(inner), name, size),
            _ => format!("{} {}", self.type_to_c(ty), name),
        };

        self.write(&decl);
    }

    fn define_global_init_start(&mut self) {
        self.write(" = ");
        self.in_variable_init = true;
    }

    fn define_global_end(&mut self) {
        self.in_variable_init = false;
        self.write(";");
        self.set_newline_pending();
    }

    fn declare_extern_function(
        &mut self,
        name: &str,
        params: &[(String, LirType)],
        return_type: &LirType,
        is_variadic: bool,
    ) {
        self.write(&format!(
            "extern {};",
            self.func_signature(name, params, return_type, is_variadic)
        ));
        self.set_newline_pending();
    }

    fn declare_function_proto(
        &mut self,
        name: &str,
        params: &[(String, LirType)],
        return_type: &LirType,
        is_variadic: bool,
    ) {
        self.write(&format!(
            "{};",
            self.func_signature(name, params, return_type, is_variadic)
        ));
        self.set_newline_pending();
    }

    fn begin_function(
        &mut self,
        name: &str,
        params: &[(String, LirType)],
        return_type: &LirType,
        is_variadic: bool,
    ) {
        if !self.output.ends_with("\n\n") {
            self.output.push('\n');
        }
        self.pending_newline = false;
        self.write(&format!(
            "{} {{",
            self.func_signature(name, params, return_type, is_variadic)
        ));
        self.push_indent();
        self.set_newline_pending();
    }

    fn end_function(&mut self) {
        self.pop_indent();
        self.write("}");
        self.set_newline_pending();
        self.output.push('\n');
    }

    fn stmt_var_decl(&mut self, name: &str, ty: &LirType, has_init: bool) {
        let decl = match ty {
            LirType::Array(inner, size) => format!("{} {}[{}]", self.type_to_c(inner), name, size),
            _ => format!("{} {}", self.type_to_c(ty), name),
        };

        self.write(&decl);

        if has_init {
            self.write(" = ");
            self.in_variable_init = true;
        } else {
            self.write(";");
            self.set_newline_pending();
        }
    }

    fn stmt_var_init_end(&mut self) {
        self.in_variable_init = false;
        self.write(";");
        self.set_newline_pending();
    }

    fn stmt_assign_start(&mut self, _lhs_is_ptr: bool) {
        self.write(" = ");
    }

    fn stmt_assign_end(&mut self) {
        self.write(";");
        self.set_newline_pending();
    }

    fn stmt_return_start(&mut self) {
        self.write("return ");
    }

    fn stmt_return_end(&mut self) {
        self.write(";");
        self.set_newline_pending();
    }

    fn stmt_break(&mut self) {
        self.write("break;");
        self.set_newline_pending();
    }

    fn stmt_continue(&mut self) {
        self.write("continue;");
        self.set_newline_pending();
    }

    fn stmt_expr_end(&mut self) {
        self.write(";");
        self.set_newline_pending();
    }

    fn begin_block(&mut self) {
        self.write("{");
        self.push_indent();
        self.set_newline_pending();
    }

    fn end_block(&mut self) {
        self.pop_indent();
        self.write("}");
        self.set_newline_pending();
    }

    fn begin_if(&mut self) {
        self.write("if (");
    }

    fn begin_if_body(&mut self) {
        self.write(") ");
    }

    fn begin_else(&mut self) {
        self.write("else ");
    }

    fn end_if(&mut self) {}

    fn begin_while(&mut self) {
        self.write("while (");
    }

    fn begin_while_body(&mut self) {
        self.write(") ");
    }

    fn end_while(&mut self) {}

    fn begin_switch(&mut self) {
        self.write("switch (");
    }

    fn begin_switch_body(&mut self) {
        self.write(") {");
        self.push_indent();
        self.set_newline_pending();
    }

    fn begin_case(&mut self, lit: &LirLiteral) {
        self.write(&format!("case {}:", self.literal_to_c(lit)));
        self.push_indent();
        self.set_newline_pending();
    }

    fn begin_default(&mut self) {
        self.write("default:");
        self.push_indent();
        self.set_newline_pending();
    }

    fn end_case(&mut self) {
        self.write("break;");
        self.pop_indent();
        self.set_newline_pending();
    }

    fn end_switch(&mut self) {
        self.pop_indent();
        self.write("}");
        self.set_newline_pending();
    }

    fn expr_array_init_start(&mut self, ty_opt: Option<&LirType>) {
        if self.in_variable_init {
            self.write("{ ");
        } else {
            if let Some(inner_ty) = ty_opt {
                let type_str = self.type_to_c(inner_ty);
                self.write(&format!("({}[]) {{ ", type_str));
            } else {
                self.write("(long long[]) { ");
            }
        }
    }

    fn expr_array_init_sep(&mut self) {
        self.write(", ");
    }

    fn expr_array_init_end(&mut self) {
        self.write(" }");
    }

    fn expr_struct_init_start(&mut self, struct_name: &str) {
        self.write(&format!("(struct {}){{ ", struct_name));
    }

    fn expr_struct_init_field_start(&mut self, field_name: &str) {
        self.write(&format!(".{} = ", field_name));
    }

    fn expr_struct_init_sep(&mut self) {
        self.write(", ");
    }

    fn expr_struct_init_end(&mut self) {
        self.write(" }");
    }

    fn expr_lit(&mut self, lit: &LirLiteral) {
        self.write(&self.literal_to_c(lit));
    }

    fn expr_ident(&mut self, name: &str) {
        self.write(name);
    }

    fn expr_binary_start(&mut self, _op: BinaryOp) {
        self.write("(");
        self.init_state_stack.push(self.in_variable_init);
        self.in_variable_init = false;
    }
    fn expr_binary_mid(&mut self, op: BinaryOp) {
        self.write(&format!(" {} ", self.binary_op_to_c(op)));
    }
    fn expr_binary_end(&mut self) {
        self.write(")");
        if let Some(prev) = self.init_state_stack.pop() {
            self.in_variable_init = prev;
        }
    }

    fn expr_unary_start(&mut self, op: UnaryOp) {
        self.write(&format!("({}", self.unary_op_to_c(op)));
    }
    fn expr_unary_end(&mut self) {
        self.write(")");
    }

    fn expr_call_start(&mut self, name: &str) {
        self.write(&format!("{}(", name));
        self.init_state_stack.push(self.in_variable_init);
        self.in_variable_init = false;
    }
    fn expr_call_arg_sep(&mut self) {
        self.write(", ");
    }
    fn expr_call_end(&mut self) {
        self.write(")");

        if let Some(prev_state) = self.init_state_stack.pop() {
            self.in_variable_init = prev_state;
        }
    }

    fn expr_member(&mut self, field: &str, is_pointer: bool) {
        self.write(if is_pointer { "->" } else { "." });
        self.write(field);
    }

    fn expr_sizeof(&mut self, ty: &LirType) {
        let type_str = self.type_to_c(ty);
        self.write(&format!("sizeof({})", type_str));
    }

    fn expr_index_start(&mut self) {
        self.write("[");
    }
    fn expr_index_end(&mut self) {
        self.write("]");
    }

    fn expr_cast_start(&mut self, target_ty: &LirType) {
        self.write(&format!("({})", self.type_to_c(target_ty)));
    }
    fn expr_cast_end(&mut self) {}

    fn expr_deref_start(&mut self) {
        self.write("(*");
    }
    fn expr_deref_end(&mut self) {
        self.write(")");
    }

    fn expr_addrof_start(&mut self) {
        self.write("(&");
    }
    fn expr_addrof_end(&mut self) {
        self.write(")");
    }

    fn expr_ternary_mid1(&mut self) {
        self.write(" ? ");
    }
    fn expr_ternary_mid2(&mut self) {
        self.write(" : ");
    }

    fn expr_is_start(&mut self) {
        self.write("(");
    }

    fn expr_is_end(&mut self, _ty: &LirType) {
        self.write(")");
    }
}
