use crate::hir::FlatProgram;
use abyss_parser::ast::{Expr, FunctionBody, FunctionDef, Program, Stmt};

pub struct Flattener {
    path: Vec<String>,
    output: FlatProgram,
}

impl Flattener {
    pub fn new() -> Self {
        Self {
            path: Vec::new(),
            output: FlatProgram::new(),
        }
    }

    pub fn flatten(mut self, program: Program) -> FlatProgram {
        self.visit_program(program);
        self.output
    }

    fn visit_program(&mut self, program: Program) {
        self.process_modules(program.modules);
        self.process_top_level_functions(program.functions);
    }

    fn process_modules(&mut self, modules: Vec<(String, Program, bool)>) {
        for (mod_name, sub_program, _) in modules {
            self.path.push(mod_name);
            self.visit_program(sub_program);
            self.path.pop();
        }
    }

    fn process_top_level_functions(&mut self, functions: Vec<FunctionDef>) {
        for func in functions {
            self.visit_function(func);
        }
    }

    fn visit_function(&mut self, mut func: FunctionDef) {
        let original_name = func.name.clone();
        let mangled_name = self.generate_mangled_name(&original_name);

        func.name = mangled_name.clone();

        if let FunctionBody::UserDefined(ref mut stmts) = func.body {
            self.process_function_body(stmts, &original_name, &mangled_name);
        }

        self.output.functions.push(func);
    }

    fn generate_mangled_name(&self, name: &str) -> String {
        if self.path.is_empty() {
            name.to_string()
        } else {
            format!("{}__{}", self.path.join("__"), name)
        }
    }

    fn process_function_body(
        &mut self,
        stmts: &mut Vec<Stmt>,
        original_name: &str,
        mangled_name: &str,
    ) {
        let (inner_funcs, mut cleaned_stmts, renames) =
            self.isolate_inner_functions(stmts, mangled_name);

        self.apply_renames(&mut cleaned_stmts, &renames);

        *stmts = cleaned_stmts;

        self.recurse_on_inner_functions(inner_funcs, original_name);
    }

    fn isolate_inner_functions(
        &self,
        stmts: &mut Vec<Stmt>,
        parent_mangled_name: &str,
    ) -> (Vec<FunctionDef>, Vec<Stmt>, Vec<(String, String)>) {
        let mut inner_funcs = Vec::new();
        let mut cleaned_stmts = Vec::new();
        let mut renames = Vec::new();

        for stmt in stmts.drain(..) {
            match stmt {
                Stmt::FunctionDef(inner_func) => {
                    let old_name = inner_func.name.clone();
                    let new_name = format!("{}__{}", parent_mangled_name, old_name);

                    renames.push((old_name, new_name));
                    inner_funcs.push(*inner_func);
                }
                _ => cleaned_stmts.push(stmt),
            }
        }

        (inner_funcs, cleaned_stmts, renames)
    }

    fn recurse_on_inner_functions(
        &mut self,
        inner_funcs: Vec<FunctionDef>,
        parent_original_name: &str,
    ) {
        self.path.push(parent_original_name.to_string());
        for inner_func in inner_funcs {
            self.visit_function(inner_func);
        }
        self.path.pop();
    }

    fn apply_renames(&self, stmts: &mut [Stmt], renames: &[(String, String)]) {
        if renames.is_empty() {
            return;
        }
        for stmt in stmts {
            self.rename_in_stmt(stmt, renames);
        }
    }

    fn rename_in_stmt(&self, stmt: &mut Stmt, renames: &[(String, String)]) {
        match stmt {
            Stmt::Let(_, _, Some(expr))
            | Stmt::Const(_, _, Some(expr))
            | Stmt::Ret(expr)
            | Stmt::Expr(expr) => self.rename_in_expr(expr, renames),

            Stmt::Assign(lhs, rhs) => {
                self.rename_in_expr(lhs, renames);
                self.rename_in_expr(rhs, renames);
            }

            Stmt::If(cond, then_branch, else_branch) => {
                self.rename_in_expr(cond, renames);
                self.rename_in_stmt(then_branch, renames);
                if let Some(else_b) = else_branch {
                    self.rename_in_stmt(else_b, renames);
                }
            }

            Stmt::While(cond, body) => {
                self.rename_in_expr(cond, renames);
                self.rename_in_stmt(body, renames);
            }

            Stmt::Block(stmts) => self.apply_renames(stmts, renames),

            _ => {}
        }
    }

    fn rename_in_expr(&self, expr: &mut Expr, renames: &[(String, String)]) {
        match expr {
            Expr::Ident(path) => self.try_rename_ident(path, renames),

            Expr::Call(callee, args, _) => {
                self.rename_in_expr(callee, renames);
                for arg in args {
                    self.rename_in_expr(arg, renames);
                }
            }

            Expr::Binary(left, _, right) => {
                self.rename_in_expr(left, renames);
                self.rename_in_expr(right, renames);
            }

            Expr::Unary(_, operand) => self.rename_in_expr(operand, renames),

            _ => {}
        }
    }

    fn try_rename_ident(&self, path: &mut Vec<String>, renames: &[(String, String)]) {
        if path.len() == 1 {
            for (old, new) in renames {
                if &path[0] == old {
                    path[0] = new.clone();
                    return;
                }
            }
        }
    }
}
