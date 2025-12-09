// use crate::target::{Target, Type};

// pub struct CTarget {
//     buffer: String,
//     indent_level: usize,
//     is_first_arg: bool,
// }

// impl CTarget {
//     pub fn new() -> Self {
//         Self {
//             buffer: String::new(),
//             indent_level: 0,
//             is_first_arg: true,
//         }
//     }

//     pub fn get_generated_code(&self) -> &str {
//         &self.buffer
//     }

//     fn indent(&self) -> String {
//         "    ".repeat(self.indent_level)
//     }

//     fn type_to_c(&self, ty: &Type) -> String {
//         match ty {
//             Type::I32 => "int".to_string(),
//             Type::U32 => "unsigned int".to_string(),
//             Type::F32 => "float".to_string(),
//             Type::Void => "void".to_string(),

//             Type::Ptr(inner) => format!("{}*", self.type_to_c(inner)),
//         }
//     }
// }

// impl Target for CTarget {
//     fn generate_code(&mut self) -> String {
//         self.buffer.clone()
//     }

//     fn start_program(&mut self) {
//         self.buffer.clear();
//         self.buffer.push_str("#include <stdio.h>\n");
//         self.buffer.push_str("#include <stdlib.h>\n\n");
//     }

//     fn end_program(&mut self) {
//         self.buffer.push_str("// End of generated code\n");
//     }

//     fn start_function(&mut self, name: &str, ret_type: Type) {
//         let c_type = self.type_to_c(&ret_type);
//         self.buffer.push_str(&format!("{} {}(", c_type, name));

//         self.is_first_arg = true;
//     }

//     fn add_function_arg(&mut self, name: &str, ty: Type) {
//         if !self.is_first_arg {
//             self.buffer.push_str(", ");
//         }

//         let c_type = self.type_to_c(&ty);
//         self.buffer.push_str(&format!("{} {}", c_type, name));

//         self.is_first_arg = false;
//     }

//     fn start_function_body(&mut self) {
//         self.buffer.push_str(") {\n");
//         self.indent_level += 1;
//     }

//     fn end_function(&mut self) {
//         self.indent_level -= 1;
//         self.buffer.push_str("}\n\n");
//     }
//     fn return_value(&mut self, value: &str, _ty: Type) {
//         let indent = self.indent();
//         if value.is_empty() {
//             self.buffer.push_str(&format!("{}return;\n", indent));
//         } else {
//             self.buffer
//                 .push_str(&format!("{}return {};\n", indent, value));
//         }
//     }
// }
