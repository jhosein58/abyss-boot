use crate::target::{Target, Type};
use abyss_parser::ast::{BinaryOp, Expr, Function, Lit, Program, Stmt, UnaryOp};

pub struct Director<'a, T: Target> {
    target: &'a mut T,
}

impl<'a, T: Target> Director<'a, T> {
    pub fn new(target: &'a mut T) -> Self {
        Self { target }
    }

    pub fn process_program(&mut self, program: &Program) {
        self.target.start_program();

        for func in &program.functions {
            let ret_ty = func
                .return_type
                .as_ref()
                .map_or(Type::Void, |s| self.map_type(s));

            let mut params = Vec::new();
            for (p_name, p_type_str) in &func.params {
                params.push((p_name.clone(), self.map_type(p_type_str)));
            }

            self.target.predefine_function(&func.name, &params, ret_ty);
        }

        for func in &program.functions {
            self.compile_function(func);
        }

        self.target.end_program();
    }

    fn compile_function(&mut self, func: &Function) {
        let ret_ty = func
            .return_type
            .as_ref()
            .map_or(Type::Void, |s| self.map_type(s));

        self.target.start_function(&func.name);

        for (param_name, _) in &func.params {
            self.target.add_function_param(param_name);
        }

        self.target.start_function_body();
        self.compile_statements(&func.body);

        if ret_ty == Type::Void {
            self.target.return_void();
        }

        self.target.end_function();
    }

    fn compile_statements(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            self.compile_statement(stmt);
        }
    }

    fn compile_statement(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Ret(expr) => {
                let value = self.compile_expr(expr);
                self.target.return_value(value);
            }
            Stmt::Let(name, expr) => {
                let inferred_type = self.infer_expr_type(expr);
                self.target.declare_variable(name, inferred_type);
                let value = self.compile_expr(expr);
                self.target.define_variable(name, value);
            }
            Stmt::Assign(name, expr) => {
                let value = self.compile_expr(expr);
                self.target.assign_variable(name, value);
            }
            Stmt::Expr(expr) => {
                self.compile_expr(expr);
            }
            Stmt::If(cond_expr, then_stmts, else_stmts) => {
                let condition = self.compile_expr(cond_expr);

                let then_block = self.target.create_block();
                let merge_block = self.target.create_block();

                let else_block = if else_stmts.is_some() {
                    self.target.create_block()
                } else {
                    merge_block
                };

                self.target.branch(condition, then_block, else_block);

                self.target.switch_to_block(then_block);
                self.target.seal_block(then_block);
                self.compile_statements(then_stmts);
                if !self.ends_with_return(then_stmts) {
                    self.target.jump(merge_block);
                }

                if let Some(else_body) = else_stmts {
                    self.target.switch_to_block(else_block);
                    self.target.seal_block(else_block);
                    self.compile_statements(else_body);
                    if !self.ends_with_return(else_body) {
                        self.target.jump(merge_block);
                    }
                }

                self.target.switch_to_block(merge_block);
                self.target.seal_block(merge_block);
            }
            Stmt::While(cond_expr, body_stmts) => {
                let header_block = self.target.create_block();
                let body_block = self.target.create_block();
                let exit_block = self.target.create_block();

                self.target.jump(header_block);

                self.target.switch_to_block(header_block);
                let condition = self.compile_expr(cond_expr);
                self.target.branch(condition, body_block, exit_block);

                self.target.switch_to_block(body_block);
                self.target.seal_block(body_block);
                self.compile_statements(body_stmts);

                self.target.jump(header_block);

                self.target.seal_block(header_block);

                self.target.switch_to_block(exit_block);
                self.target.seal_block(exit_block);
            }
        }
    }

    fn compile_expr(&mut self, expr: &Expr) -> T::Value {
        match expr {
            Expr::Lit(lit) => match *lit {
                Lit::Int(val) => self.target.translate_lit_int(val),
                Lit::Float(val) => self.target.translate_lit_float(val),
                Lit::Bool(val) => self.target.translate_lit_bool(val),
            },
            Expr::Ident(name) => self.target.translate_ident(name),
            Expr::Binary(lhs, op, rhs) => {
                let left_val = self.compile_expr(lhs);
                let right_val = self.compile_expr(rhs);
                self.target.translate_binary_op(*op, left_val, right_val)
            }
            Expr::Unary(op, expr) => {
                let val = self.compile_expr(expr);
                self.target.translate_unary_op(*op, val)
            }
            Expr::Call(name, args) => {
                let mut compiled_args = Vec::new();
                for arg in args {
                    compiled_args.push(self.compile_expr(arg));
                }
                self.target.call_function(name, compiled_args)
            }
        }
    }

    fn map_type(&self, type_str: &str) -> Type {
        match type_str {
            "i64" | "int" => Type::Int,
            "f64" | "float" => Type::Float,
            "bool" => Type::Bool,
            "void" => Type::Void,
            _ => panic!("Unknown type: {}", type_str),
        }
    }

    fn infer_expr_type(&self, expr: &Expr) -> Type {
        match expr {
            Expr::Lit(Lit::Float(_)) => Type::Float,
            Expr::Lit(Lit::Bool(_)) => Type::Bool,
            Expr::Lit(Lit::Int(_)) => Type::Int,

            Expr::Binary(_, op, _) => match op {
                BinaryOp::Eq
                | BinaryOp::Neq
                | BinaryOp::Lt
                | BinaryOp::Gt
                | BinaryOp::Lte
                | BinaryOp::Gte
                | BinaryOp::And
                | BinaryOp::Or => Type::Bool,
                _ => Type::Int,
            },

            Expr::Ident(_) => Type::Int,

            Expr::Unary(op, _) => match op {
                UnaryOp::Not => Type::Bool,
                UnaryOp::Neg => Type::Int,
            },

            Expr::Call(_, _) => Type::Int,
        }
    }

    fn ends_with_return(&self, stmts: &[Stmt]) -> bool {
        if let Some(last) = stmts.last() {
            matches!(last, Stmt::Ret(_))
        } else {
            false
        }
    }
}
