// use crate::{
//     parser::ast::{Expression, Program, Statement},
//     targets::CompilationTarget,
// };

// pub struct Director<T: CompilationTarget> {
//     target: T,
// }

// impl<T: CompilationTarget> Director<T> {
//     pub fn new(target: T) -> Self {
//         Self { target }
//     }

//     pub fn conclude(self) -> T {
//         self.target
//     }

//     pub fn run(&mut self, program: Program) {
//         for stmt in program.statements {
//             self.process_statement(stmt);
//         }

//         self.target.build_entry_point("main");
//     }

//     fn process_statement(&mut self, stmt: Statement) {
//         match stmt {
//             Statement::Let { name, value } => {
//                 let val_output = self.process_expression(value);
//                 self.target.var_declare(&name, val_output);
//             }

//             Statement::Assign { name, value } => {
//                 let val_output = self.process_expression(value);
//                 self.target.var_assign(&name, val_output);
//             }

//             Statement::Return(expr) => {
//                 let val_output = self.process_expression(expr);
//                 self.target.ret(val_output);
//             }

//             Statement::Expression(expr) => {
//                 let val_output = self.process_expression(expr);
//                 self.target.expr_stmt(val_output);
//             }

//             Statement::Function { name, params, body } => {
//                 let param_names: Vec<&str> = params.iter().map(|p| p.name.as_str()).collect();

//                 self.target.func_def(&name, &param_names);

//                 for body_stmt in body {
//                     self.process_statement(body_stmt);
//                 }

//                 self.target.func_end();
//             }
//         }
//     }

//     fn process_expression(&mut self, expr: Expression) -> T::Output {
//         match expr {
//             Expression::Number(n) => self.target.number(n),

//             Expression::String(s) => self.target.string(&s),

//             Expression::Identifier(name) => self.target.variable(&name),

//             Expression::Infix {
//                 left,
//                 operator,
//                 right,
//             } => {
//                 let l_out = self.process_expression(*left);
//                 let r_out = self.process_expression(*right);

//                 match operator.as_str() {
//                     "+" => self.target.add(l_out, r_out),
//                     "-" => self.target.sub(l_out, r_out),
//                     "*" => self.target.mul(l_out, r_out),
//                     "/" => self.target.div(l_out, r_out),
//                     _ => panic!("Operator {} not supported yet", operator),
//                 }
//             }

//             Expression::Call {
//                 function,
//                 arguments,
//             } => {
//                 let func_name = match *function {
//                     Expression::Identifier(name) => name,
//                     _ => panic!("Complex function calls (closures) not supported in C target yet"),
//                 };

//                 let mut compiled_args = Vec::new();
//                 for arg in arguments {
//                     compiled_args.push(self.process_expression(arg));
//                 }

//                 self.target.call(&func_name, compiled_args)
//             }
//         }
//     }
// }
