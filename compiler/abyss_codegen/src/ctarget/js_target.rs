// use crate::target::Target;
// use abyss_analyzer::lir::{LirLiteral, LirType};
// use abyss_parser::ast::{BinaryOp, UnaryOp};
// use std::collections::HashMap;

// pub struct JsTarget {
//     output: String,
//     indent_level: usize,
//     pending_newline: bool,
//     in_variable_init: bool,
//     is_lhs_stack: Vec<bool>,
//     struct_layouts: HashMap<String, HashMap<String, usize>>,
//     struct_sizes: HashMap<String, usize>,
//     current_decl_size: usize,
//     in_memcpy_init: bool,
//     pub assign_needs_closing_stack: Vec<bool>,
//     pub assignment_mode_stack: Vec<bool>,
//     pub needs_closing_paren_stack: Vec<bool>,
// }

// impl JsTarget {
//     pub fn new() -> Self {
//         Self {
//             output: String::new(),
//             indent_level: 0,
//             pending_newline: false,
//             in_variable_init: false,
//             is_lhs_stack: Vec::new(),
//             struct_layouts: HashMap::new(),
//             struct_sizes: HashMap::new(),
//             current_decl_size: 0,
//             in_memcpy_init: false,
//             assign_needs_closing_stack: Vec::new(),
//             assignment_mode_stack: Vec::new(),
//             needs_closing_paren_stack: Vec::new(),
//         }
//     }

//     fn push_indent(&mut self) {
//         self.indent_level += 1;
//     }
//     fn pop_indent(&mut self) {
//         if self.indent_level > 0 {
//             self.indent_level -= 1;
//         }
//     }

//     fn write(&mut self, text: &str) {
//         if self.pending_newline {
//             self.output.push('\n');
//             self.output.push_str(&"    ".repeat(self.indent_level));
//             self.pending_newline = false;
//         }
//         self.output.push_str(text);
//     }

//     fn _set_newline_pending(&mut self) {
//         self.pending_newline = true;
//     }

//     fn get_type_tag(&self, ty: &LirType) -> String {
//         match ty {
//             LirType::I64 => "'i64'".to_string(),
//             LirType::F64 => "'f64'".to_string(),
//             LirType::U8 => "'u8'".to_string(),
//             LirType::Bool
//             | LirType::Pointer(_)
//             | LirType::Array(_, _)
//             | LirType::FunctionPtr(_, _) => "'i32'".to_string(),
//             LirType::Struct(_) => "'struct'".to_string(),
//             LirType::Void => "'void'".to_string(),
//         }
//     }

//     fn get_type_size(&self, ty: &LirType) -> usize {
//         match ty {
//             LirType::I64 | LirType::F64 => 8,
//             LirType::Bool | LirType::U8 => 1,
//             LirType::Pointer(_) | LirType::FunctionPtr(_, _) => 4,
//             LirType::Array(inner, size) => self.get_type_size(inner) * size,
//             LirType::Struct(name) => *self.struct_sizes.get(name).unwrap_or(&0),
//             LirType::Void => 0,
//         }
//     }

//     fn literal_to_js(&self, lit: &LirLiteral) -> String {
//         match lit {
//             LirLiteral::Int(i) => format!("{}", i),
//             LirLiteral::Float(f) => format!("{}", f),
//             LirLiteral::Byte(b) => format!("{}", b),
//             LirLiteral::Bool(b) => {
//                 if *b {
//                     "1".to_string()
//                 } else {
//                     "0".to_string()
//                 }
//             }
//             LirLiteral::Null => "0".to_string(),
//             LirLiteral::Array(_) => "0".to_string(),
//         }
//     }

//     fn binary_op_to_js(&self, op: BinaryOp) -> &'static str {
//         match op {
//             BinaryOp::And => "&&",
//             BinaryOp::Or => "||",
//             BinaryOp::Eq => "==",
//             BinaryOp::Neq => "!=",
//             BinaryOp::Add => "+",
//             BinaryOp::Sub => "-",
//             BinaryOp::Mul => "*",
//             BinaryOp::Div => "/",
//             BinaryOp::Mod => "%",
//             BinaryOp::Lt => "<",
//             BinaryOp::Gt => ">",
//             BinaryOp::Lte => "<=",
//             BinaryOp::Gte => ">=",
//             BinaryOp::BitAnd => "&",
//             BinaryOp::BitOr => "|",
//             BinaryOp::BitXor => "^",
//             BinaryOp::Shl => "<<",
//             BinaryOp::Shr => ">>",
//             BinaryOp::Assign => "=",
//         }
//     }

//     fn unary_op_to_js(&self, op: UnaryOp) -> &'static str {
//         match op {
//             UnaryOp::Not => "!",
//             UnaryOp::Neg => "-",
//             UnaryOp::BitNot => "~",
//         }
//     }
// }

// impl Target for JsTarget {
//     fn emit(&mut self) -> String {
//         self.output.clone()
//     }

//     fn start_program(&mut self) {
//         self.output.clear();
//         self.output.push_str(
//             r#"
// const MEM_SIZE = 64 * 1024 * 1024;
// const MEM = new ArrayBuffer(MEM_SIZE);
// const DV = new DataView(MEM);
// const U8 = new Uint8Array(MEM);
// let HEAP_PTR = 8;
// function _malloc(size) {
//     let ptr = HEAP_PTR;
//     HEAP_PTR += size;
//     if (HEAP_PTR % 8 !== 0) HEAP_PTR += (8 - (HEAP_PTR % 8));
//     return ptr;
// }
// function _free(ptr) { }
// function _realloc(ptr, new_size) {
//     let new_ptr = _malloc(new_size);
//     _memcpy(new_ptr, ptr, new_size);
//     return new_ptr;
// }
// function _addr(v) { return (v instanceof Ref) ? v.a : v; }

// Number.prototype.add = function(off) { return new Ref(this + off, 'u8'); };

// class Ref {
//     constructor(addr, type) { this.a = addr | 0; this.t = type || 'i64'; }
//     valueOf() {
//         switch(this.t) {
//             case 'i64': return Number(DV.getBigInt64(this.a, true));
//             case 'f64': return DV.getFloat64(this.a, true);
//             case 'u8': return DV.getUint8(this.a);
//             default: return DV.getInt32(this.a, true);
//         }
//     }
//     set(val) {
//         switch(this.t) {
//             case 'i64': DV.setBigInt64(this.a, BigInt(val), true); break;
//             case 'f64': DV.setFloat64(this.a, val, true); break;
//             case 'u8': DV.setUint8(this.a, val); break;
//             default: DV.setInt32(this.a, val, true); break;
//         }
//         return val;
//     }
//     add(off) { return new Ref(this.a + off, this.t); }
//     cast(type) { return new Ref(this.a, type); }
//     get addr() { return this.a; }
// }

// function _memcpy(dest, src, size) {
//     let d = _addr(dest);
//     let s = _addr(src);
//     new Uint8Array(MEM).set(new Uint8Array(MEM, s, size), d);
//     return d;
// }
// function _alloc_struct(vals) {
//     let ptr = _malloc(vals.length * 8);
//     for(let i=0; i<vals.length; i++) {
//         DV.setBigInt64(ptr + i*8, BigInt(vals[i]), true);
//     }
//     return ptr;
// }
// function _alloc_array(vals) {
//     let ptr = _malloc(vals.length);
//     for(let i=0; i<vals.length; i++) {
//         if(vals[i] < 256) U8[ptr+i] = vals[i];
//         else DV.setBigInt64(ptr + i*8, BigInt(vals[i]), true);
//     }
//     return ptr;
// }
// const _OFFSETS = {};
// function _str_from_ptr(ptr) {
//     ptr = _addr(ptr);
//     let s = "";
//     while(true) {
//         let c = U8[ptr];
//         if(c === 0) break;
//         s += String.fromCharCode(c);
//         ptr++;
//     }
//     return s;
// }
// const print = (fmt, val) => {
//     let s = _str_from_ptr(fmt);
//     if(val !== undefined) console.log(s, _addr(val));
//     else console.log(s);
// };
// "#,
//         );
//     }

//     fn end_program(&mut self) {
//         self.output.push_str("\napp_main();\n");
//     }

//     fn define_struct(&mut self, name: &str, fields: &[(String, LirType)]) {
//         let mut offset = 0;
//         let mut field_map = HashMap::new();
//         self.write(&format!("// {}\n", name));
//         for (f_name, f_type) in fields {
//             let key = format!("{}_{}", name, f_name);
//             self.write(&format!("_OFFSETS['{}'] = {};\n", key, offset));
//             field_map.insert(f_name.clone(), offset);
//             offset += self.get_type_size(f_type);
//         }
//         self.struct_layouts.insert(name.to_string(), field_map);
//         self.struct_sizes.insert(name.to_string(), offset);
//         self.output.push('\n');
//     }

//     fn define_global_start(&mut self, name: &str, ty: &LirType, _: bool) {
//         let size = self.get_type_size(ty);
//         self.write(&format!("const {} = _malloc({});\n", name, size));
//         self.write(&format!(
//             "new Ref({}, '{}').set(",
//             name,
//             self.get_type_tag(ty)
//         ));
//     }

//     fn define_global_init_start(&mut self) {}

//     fn define_global_end(&mut self) {
//         self.write(");\n");
//     }

//     fn declare_extern_function(&mut self, _: &str, _: &[(String, LirType)], _: &LirType) {}
//     fn declare_function_proto(&mut self, _: &str, _: &[(String, LirType)], _: &LirType) {}

//     fn begin_function(&mut self, name: &str, params: &[(String, LirType)], _: &LirType) {
//         let args = params
//             .iter()
//             .map(|p| p.0.clone())
//             .collect::<Vec<_>>()
//             .join(", ");
//         self.write(&format!("function {}({}) {{\n", name, args));
//         self.push_indent();
//     }

//     fn end_function(&mut self) {
//         self.pop_indent();
//         self.write("}\n");
//     }
//     fn stmt_var_decl(&mut self, name: &str, ty: &LirType, has_init: bool) {
//         self.current_decl_size = self.get_type_size(ty);
//         match ty {
//             LirType::Struct(sname) => {
//                 let size = self.struct_sizes.get(sname).unwrap_or(&0);
//                 self.write(&format!("let {} = _malloc({});\n", name, size));
//                 if has_init {
//                     self.write(&format!("_memcpy({}, ", name));
//                     self.in_variable_init = true;
//                     self.in_memcpy_init = true;
//                 }
//             }
//             _ => {
//                 self.write(&format!("let {}", name));
//                 if has_init {
//                     self.write(" = ");
//                     self.in_variable_init = true;
//                     self.in_memcpy_init = false;
//                 } else {
//                     self.write(" = 0;\n");
//                 }
//             }
//         }
//     }
//     fn stmt_var_init_end(&mut self) {
//         if self.in_variable_init {
//             if self.in_memcpy_init {
//                 self.write(&format!(", {});\n", self.current_decl_size));
//             } else {
//                 self.write(";\n");
//             }
//         }
//         self.in_variable_init = false;
//         self.in_memcpy_init = false;
//     }

//     fn stmt_assign_start(&mut self, lhs_is_ptr: bool) {
//         let is_complex_lvalue = self.output.trim_end().ends_with(')');

//         let use_set = lhs_is_ptr || is_complex_lvalue;

//         self.is_lhs_stack.push(use_set);

//         if use_set {
//             self.write(".set(");
//         } else {
//             self.write(" = ");
//         }
//     }

//     fn stmt_assign_end(&mut self) {
//         let use_set = self.is_lhs_stack.pop().unwrap_or(false);
//         if use_set {
//             self.write(");\n");
//         } else {
//             self.write(";\n");
//         }
//     }
//     fn stmt_return_start(&mut self) {
//         self.write("return Number(");
//     }
//     fn stmt_return_end(&mut self) {
//         self.write(");\n");
//     }
//     fn stmt_break(&mut self) {
//         self.write("break;\n");
//     }
//     fn stmt_continue(&mut self) {
//         self.write("continue;\n");
//     }
//     fn stmt_expr_end(&mut self) {
//         self.write(";\n");
//     }

//     fn begin_block(&mut self) {
//         self.write("{\n");
//         self.push_indent();
//     }
//     fn end_block(&mut self) {
//         self.pop_indent();
//         self.write("}\n");
//     }
//     fn begin_if(&mut self) {
//         self.write("if (");
//     }
//     fn begin_if_body(&mut self) {
//         self.write(") ");
//     }
//     fn begin_else(&mut self) {
//         self.write("else ");
//     }
//     fn end_if(&mut self) {}
//     fn begin_while(&mut self) {
//         self.write("while (");
//     }
//     fn begin_while_body(&mut self) {
//         self.write(") ");
//     }
//     fn end_while(&mut self) {}

//     fn begin_switch(&mut self) {
//         self.write("switch (");
//     }
//     fn begin_switch_body(&mut self) {
//         self.write(") {\n");
//         self.push_indent();
//     }
//     fn begin_case(&mut self, lit: &LirLiteral) {
//         self.write(&format!("case {}:\n", self.literal_to_js(lit)));
//     }
//     fn begin_default(&mut self) {
//         self.write("default:\n");
//     }
//     fn end_case(&mut self) {}
//     fn end_switch(&mut self) {
//         self.pop_indent();
//         self.write("}\n");
//     }

//     fn expr_array_init_start(&mut self, _: Option<&LirType>) {
//         self.write("_alloc_array([");
//     }
//     fn expr_array_init_sep(&mut self) {
//         self.write(", ");
//     }
//     fn expr_array_init_end(&mut self) {
//         self.write("])");
//     }

//     fn expr_struct_init_start(&mut self, _: &str) {
//         self.write("_alloc_struct([");
//     }
//     fn expr_struct_init_field_start(&mut self, _: &str) {}
//     fn expr_struct_init_sep(&mut self) {
//         self.write(", ");
//     }
//     fn expr_struct_init_end(&mut self) {
//         self.write("])");
//     }

//     fn expr_lit(&mut self, lit: &LirLiteral) {
//         self.write(&self.literal_to_js(lit));
//     }
//     fn expr_ident(&mut self, name: &str) {
//         self.write(name);
//     }
//     fn expr_binary_start(&mut self, op: BinaryOp) {
//         if op != BinaryOp::Assign {
//             self.write("Number(");
//             self.needs_closing_paren_stack.push(true);
//         } else {
//             self.write("");
//             self.needs_closing_paren_stack.push(false);
//         }
//     }

//     fn expr_binary_mid(&mut self, op: BinaryOp) {
//         if op == BinaryOp::Assign {
//             let is_complex_lvalue = self.output.trim_end().ends_with(')');

//             if is_complex_lvalue {
//                 self.write(".set(");
//                 if let Some(last) = self.needs_closing_paren_stack.last_mut() {
//                     *last = true;
//                 }
//             } else {
//                 self.write(" = ");
//             }
//         } else {
//             self.write(&format!(") {} Number(", self.binary_op_to_js(op)));
//         }
//     }

//     fn expr_binary_end(&mut self) {
//         let needs_closing = self.needs_closing_paren_stack.pop().unwrap_or(false);

//         if needs_closing {
//             self.write(")");
//         }
//     }

//     fn expr_unary_start(&mut self, op: UnaryOp) {
//         self.write(self.unary_op_to_js(op));
//         self.write("(");
//     }
//     fn expr_unary_end(&mut self) {
//         self.write(")");
//     }

//     fn expr_call_start(&mut self, name: &str) {
//         match name {
//             "realloc" => self.write("_realloc("),
//             "free" => self.write("_free("),
//             _ => self.write(&format!("{}(", name)),
//         }
//     }
//     fn expr_call_arg_sep(&mut self) {
//         self.write(", ");
//     }
//     fn expr_call_end(&mut self) {
//         self.write(")");
//     }

//     fn expr_member(&mut self, field: &str, _: bool) {
//         let mut offset = 0;
//         let mut found = false;
//         for (_, layout) in &self.struct_layouts {
//             if let Some(off) = layout.get(field) {
//                 offset = *off;
//                 found = true;
//                 break;
//             }
//         }
//         if !found {
//             self.write(&format!("/*Err:{}*/", field));
//         } else {
//             self.write(&format!(".add({})", offset));
//         }
//     }

//     fn expr_sizeof(&mut self, ty: &LirType) {
//         self.write(&format!("{}", self.get_type_size(ty)));
//     }

//     fn expr_index_start(&mut self) {
//         self.write(".add(");
//     }
//     fn expr_index_end(&mut self) {
//         self.write(")");
//     }
//     fn expr_cast_start(&mut self, _: &LirType) {
//         self.write("Number(");
//     }
//     fn expr_cast_end(&mut self) {
//         self.write(")");
//     }

//     fn expr_deref_start(&mut self) {
//         self.write("new Ref(");
//     }
//     fn expr_deref_end(&mut self) {
//         self.write(", 'i64')");
//     }

//     fn expr_addrof_start(&mut self) {
//         self.write("((_t = ");
//     }
//     fn expr_addrof_end(&mut self) {
//         self.write(") instanceof Ref ? _t.addr : _t)");
//     }

//     fn expr_ternary_mid1(&mut self) {
//         self.write("?");
//     }
//     fn expr_ternary_mid2(&mut self) {
//         self.write(":");
//     }
// }
