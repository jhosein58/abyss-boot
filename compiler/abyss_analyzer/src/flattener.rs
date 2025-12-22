use crate::hir::FlatProgram;
use abyss_parser::ast::{
    Expr, FunctionBody, FunctionDef, Lit, Program, StaticDef, Stmt, StructDef, Type,
};

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

        self.process_top_level_structs(program.structs);
        self.process_top_level_functions(program.functions);
        self.process_top_level_statics(program.statics);
    }

    fn process_top_level_structs(&mut self, structs: Vec<StructDef>) {
        for mut s in structs {
            s.name = self.generate_mangled_name(&s.name);
            self.output.structs.push(s);
        }
    }
    fn process_top_level_statics(&mut self, statics: Vec<StaticDef>) {
        for mut s in statics {
            s.name = self.generate_mangled_name(&s.name);
            self.output.statics.push(s);
        }
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

        let mut extracted_inner_funcs = Vec::new();

        if let FunctionBody::UserDefined(ref mut stmts) = func.body {
            let (inner_funcs, extracted_structs, mut cleaned_stmts, renames) =
                self.isolate_inner_functions(stmts, &mangled_name);

            extracted_inner_funcs = inner_funcs;

            for s in extracted_structs {
                self.output.structs.push(s);
            }

            self.apply_renames(&mut cleaned_stmts, &renames);
            *stmts = cleaned_stmts;
        }

        self.output.functions.push(func);

        self.path.push(original_name);
        for inner_func in extracted_inner_funcs {
            self.visit_function(inner_func);
        }
        self.path.pop();
    }

    fn generate_mangled_name(&self, name: &str) -> String {
        if self.path.is_empty() {
            name.to_string()
        } else {
            format!("{}__{}", self.path.join("__"), name)
        }
    }

    fn isolate_inner_functions(
        &self,
        stmts: &mut Vec<Stmt>,
        parent_mangled_name: &str,
    ) -> (
        Vec<FunctionDef>,
        Vec<StructDef>,
        Vec<Stmt>,
        Vec<(String, String)>,
    ) {
        let mut inner_funcs = Vec::new();
        let mut cleaned_stmts = Vec::new();
        let mut inner_structs = Vec::new();
        let mut renames = Vec::new();

        for stmt in stmts.drain(..) {
            match stmt {
                Stmt::FunctionDef(inner_func_box) => {
                    let inner_func = *inner_func_box;
                    let old_name = inner_func.name.clone();
                    let new_global_name = format!("{}__{}", parent_mangled_name, old_name);

                    renames.push((old_name, new_global_name));
                    inner_funcs.push(inner_func);
                }
                Stmt::StructDef(inner_struct_box) => {
                    let mut inner_struct = *inner_struct_box;
                    let old_name = inner_struct.name.clone();
                    let new_global_name = format!("{}__{}", parent_mangled_name, old_name);

                    inner_struct.name = new_global_name.clone();
                    renames.push((old_name, new_global_name));

                    inner_structs.push(inner_struct);
                }

                Stmt::Mod(path, Some(body_box)) => {
                    if let Some(mod_name) = path.last() {
                        let mod_prefix = format!("{}__{}", parent_mangled_name, mod_name);

                        if let Stmt::Block(mut mod_stmts) = *body_box {
                            let (
                                extracted_funcs,
                                extracted_structs,
                                cleaned_mod_stmts,
                                mod_renames,
                            ) = self.isolate_inner_functions(&mut mod_stmts, &mod_prefix);

                            inner_funcs.extend(extracted_funcs);
                            inner_structs.extend(extracted_structs);

                            renames.extend(mod_renames);

                            cleaned_stmts.push(Stmt::Block(cleaned_mod_stmts));
                        }
                    }
                }

                _ => cleaned_stmts.push(stmt),
            }
        }

        (inner_funcs, inner_structs, cleaned_stmts, renames)
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
            Stmt::Let(_, Some(ty), Some(expr)) | Stmt::Const(_, Some(ty), Some(expr)) => {
                self.rename_in_type(ty, renames);
                self.rename_in_expr(expr, renames);
            }
            Stmt::Let(_, Some(ty), None) | Stmt::Const(_, Some(ty), None) => {
                self.rename_in_type(ty, renames);
            }

            Stmt::Ret(expr) | Stmt::Expr(expr) => {
                self.rename_in_expr(expr, renames);
            }

            Stmt::Assign(lhs, rhs) => {
                self.rename_in_expr(lhs, renames);
                self.rename_in_expr(rhs, renames);
            }

            Stmt::If(cond, then_stmt, else_stmt) => {
                self.rename_in_expr(cond, renames);
                self.rename_in_stmt(then_stmt, renames);
                if let Some(else_s) = else_stmt {
                    self.rename_in_stmt(else_s, renames);
                }
            }

            Stmt::While(cond, body) => {
                self.rename_in_expr(cond, renames);
                self.rename_in_stmt(body, renames);
            }

            Stmt::Block(stmts) => {
                self.apply_renames(stmts, renames);
            }

            _ => {}
        }
    }

    fn rename_in_expr(&self, expr: &mut Expr, renames: &[(String, String)]) {
        match expr {
            Expr::Ident(path) => {
                if path.len() == 1 {
                    for (old, new) in renames {
                        if &path[0] == old {
                            path[0] = new.clone();
                            return;
                        }
                    }
                }
            }

            Expr::Call(callee, args, generics) => {
                self.rename_in_expr(callee, renames);
                for arg in args {
                    self.rename_in_expr(arg, renames);
                }
                for g in generics {
                    self.rename_in_type(g, renames);
                }
            }

            Expr::Binary(left, _, right) => {
                self.rename_in_expr(left, renames);
                self.rename_in_expr(right, renames);
            }

            Expr::Unary(_, operand) => {
                self.rename_in_expr(operand, renames);
            }

            Expr::Index(arr, idx) => {
                self.rename_in_expr(arr, renames);
                self.rename_in_expr(idx, renames);
            }

            Expr::Deref(inner) | Expr::AddrOf(inner) => {
                self.rename_in_expr(inner, renames);
            }

            Expr::Member(obj, _) => {
                self.rename_in_expr(obj, renames);
            }

            Expr::StructInit(path, fields, generics) => {
                if path.len() == 1 {
                    for (old, new) in renames {
                        if &path[0] == old {
                            path[0] = new.clone();
                            break;
                        }
                    }
                }
                for (_, val_expr) in fields {
                    self.rename_in_expr(val_expr, renames);
                }
                for g in generics {
                    self.rename_in_type(g, renames);
                }
            }

            Expr::MethodCall(obj, _, args, generics) => {
                self.rename_in_expr(obj, renames);
                for arg in args {
                    self.rename_in_expr(arg, renames);
                }
                for g in generics {
                    self.rename_in_type(g, renames);
                }
            }

            Expr::Ternary(cond, true_val, false_val) => {
                self.rename_in_expr(cond, renames);
                self.rename_in_expr(true_val, renames);
                self.rename_in_expr(false_val, renames);
            }

            Expr::Match(target, arms) => {
                self.rename_in_expr(target, renames);
                for (_, body) in arms {
                    self.rename_in_expr(body, renames);
                }
            }

            Expr::Lit(Lit::Array(exprs)) => {
                for e in exprs {
                    self.rename_in_expr(e, renames);
                }
            }

            Expr::Cast(inner, ty) => {
                self.rename_in_expr(inner, renames);
                self.rename_in_type(ty, renames);
            }

            Expr::SizeOf(ty) => {
                self.rename_in_type(ty, renames);
            }

            _ => {}
        }
    }

    fn rename_in_type(&self, ty: &mut Type, renames: &[(String, String)]) {
        match ty {
            Type::Struct(path, generics) => {
                if path.len() == 1 {
                    for (old, new) in renames {
                        if &path[0] == old {
                            path[0] = new.clone();
                            break;
                        }
                    }
                }
                for g in generics {
                    self.rename_in_type(g, renames);
                }
            }

            Type::Pointer(inner) => self.rename_in_type(inner, renames),
            Type::Array(inner, _) => self.rename_in_type(inner, renames),

            Type::Function(args, ret, _generics) => {
                for arg in args {
                    self.rename_in_type(arg, renames);
                }
                self.rename_in_type(ret, renames);
            }

            _ => {}
        }
    }
}
