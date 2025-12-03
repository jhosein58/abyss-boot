// use std::{cell::RefCell, collections::HashMap};

// use crate::parser::{
//     ast::{Expression, Program, Statement},
//     parser::Parser,
// };

// pub struct Interpreter {
//     ast: Program,
//     pub functions: RefCell<HashMap<String, Vec<Statement>>>,
// }

// impl Interpreter {
//     pub fn new(mut parser: Parser) -> Self {
//         let ast = parser.parse_program();
//         let mut interpreter = Interpreter {
//             ast,
//             functions: RefCell::new(HashMap::new()),
//         };
//         interpreter.load_functions();
//         interpreter
//     }

//     fn load_functions(&mut self) {
//         for st in self.ast.statements.iter() {
//             if let Statement::Function {
//                 name,
//                 params: _,
//                 body,
//             } = st
//             {
//                 self.functions
//                     .borrow_mut()
//                     .insert(name.clone(), body.clone());
//             }
//         }
//     }

//     pub fn run(&mut self) {
//         let mut global_env = HashMap::new();
//         self.call_function("main", &mut global_env);
//     }

//     fn call_function(&self, name: &str, env: &mut HashMap<String, f64>) -> f64 {
//         if name == "print" {
//             return 0.0;
//         }

//         let functions = self.functions.borrow();
//         let body = match functions.get(name) {
//             Some(b) => b,
//             None => panic!("Function '{}' not found", name),
//         };

//         for stmt in body {
//             if let Some(ret_val) = self.exec_statement(stmt, env) {
//                 return ret_val;
//             }
//         }

//         0.0
//     }

//     fn exec_statement(&self, stmt: &Statement, env: &mut HashMap<String, f64>) -> Option<f64> {
//         match stmt {
//             Statement::Let { name, value } => {
//                 let val = self.eval_expression(value, env);
//                 env.insert(name.clone(), val);
//                 None
//             }
//             Statement::Assign { name, value } => {
//                 let val = self.eval_expression(value, env);
//                 if env.contains_key(name) {
//                     env.insert(name.clone(), val);
//                 } else {
//                     panic!("Variable '{}' not defined before assignment", name);
//                 }
//                 None
//             }
//             Statement::Return(expr) => {
//                 let val = self.eval_expression(expr, env);
//                 Some(val)
//             }
//             Statement::Expression(expr) => {
//                 self.eval_expression(expr, env);
//                 None
//             }
//             _ => None,
//         }
//     }

//     fn eval_expression(&self, expr: &Expression, env: &mut HashMap<String, f64>) -> f64 {
//         match expr {
//             Expression::Number(n) => *n,
//             Expression::Identifier(name) => *env
//                 .get(name)
//                 .expect(&format!("Variable '{}' not found", name)),
//             Expression::Infix {
//                 left,
//                 operator,
//                 right,
//             } => {
//                 let l = self.eval_expression(left, env);
//                 let r = self.eval_expression(right, env);
//                 match operator.as_str() {
//                     "+" => l + r,
//                     "-" => l - r,
//                     "*" => l * r,
//                     "/" => l / r,
//                     _ => panic!("Unknown operator"),
//                 }
//             }
//             Expression::Call {
//                 function,
//                 arguments,
//             } => {
//                 let func_name = match &**function {
//                     Expression::Identifier(n) => n,
//                     _ => panic!("Function name must be identifier"),
//                 };

//                 if func_name == "print" {
//                     if let Some(arg_expr) = arguments.get(0) {
//                         let val = self.eval_expression(arg_expr, env);
//                         println!("{}", val);
//                         return val;
//                     }
//                 }

//                 let mut func_env = HashMap::new();

//                 self.call_function(func_name, &mut func_env)
//             }
//             _ => panic!("Unknown expression type"),
//         }
//     }
// }
