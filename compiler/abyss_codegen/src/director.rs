use std::collections::{HashMap, HashSet};

use abyss_analyzer::lir::{
    LirExpr, LirFunctionDef, LirLiteral, LirProgram, LirStmt, LirStructDef, LirType, LirUnionDef,
};
use abyss_parser::ast::UnaryOp;

use crate::target::Target;

#[derive(Clone)]
enum Definition<'a> {
    Struct(&'a LirStructDef),
    Union(&'a LirUnionDef),
}

impl<'a> Definition<'a> {
    fn name(&self) -> &str {
        match self {
            Definition::Struct(s) => &s.name,
            Definition::Union(u) => &u.name,
        }
    }
}

pub struct Director<'a, T: Target> {
    target: &'a mut T,
}

impl<'a, T: Target> Director<'a, T> {
    pub fn new(target: &'a mut T) -> Self {
        Self { target }
    }

    fn infer_array_type(&self, items: &[LirExpr]) -> Option<LirType> {
        if let Some(first) = items.first() {
            match first {
                LirExpr::Lit(LirLiteral::Int(_)) => Some(LirType::I64),
                LirExpr::Lit(LirLiteral::Byte(_)) => Some(LirType::U8),

                LirExpr::Lit(LirLiteral::Float(_)) => Some(LirType::F64),
                LirExpr::Lit(LirLiteral::Bool(_)) => Some(LirType::Bool),
                _ => Some(LirType::I64),
            }
        } else {
            Some(LirType::I64)
        }
    }

    pub fn process_program(&mut self, program: &LirProgram) {
        self.target.start_program();

        let mut all_defs = Vec::new();

        for s in &program.structs {
            all_defs.push(Definition::Struct(s));
        }
        for u in &program.unions {
            all_defs.push(Definition::Union(u));
        }
        for s in &program.union_struct_defs {
            all_defs.push(Definition::Struct(s));
        }

        let sorted_defs = self.sort_definitions_topologically(all_defs);

        for def in sorted_defs {
            match def {
                Definition::Struct(st) => {
                    self.target.define_struct(&st.name, &st.fields);
                }
                Definition::Union(u) => {
                    self.target.define_union(&u.name, &u.variants);
                }
            }
        }

        for func in &program.functions {
            let ret_ty = &func.return_type;
            let params = self.get_func_params(func);
            if func.is_extern {
                self.target.declare_extern_function(
                    &func.name,
                    func.external_name.clone(),
                    &params,
                    ret_ty,
                    func.is_variadic,
                );
            } else {
                self.target
                    .declare_function_proto(&func.name, &params, ret_ty, func.is_variadic);
            }
        }

        for glob in &program.globals {
            self.target.define_global_start(&glob.name, &glob.ty, false);
            if let Some(init_expr) = &glob.init_value {
                self.target.define_global_init_start();
                self.process_expr(init_expr);
            }
            self.target.define_global_end();
        }

        for func in &program.functions {
            if !func.is_extern {
                self.compile_function(func);
            }
        }

        self.target.end_program();
    }

    fn sort_definitions_topologically<'b>(&self, defs: Vec<Definition<'b>>) -> Vec<Definition<'b>> {
        let mut visited = HashSet::new();
        let mut temp_visited = HashSet::new();
        let mut result = Vec::new();

        let mut def_map: HashMap<String, Definition<'b>> = HashMap::new();
        for def in &defs {
            def_map.insert(def.name().to_string(), def.clone());
        }

        fn visit<'b>(
            name: &str,
            def_map: &HashMap<String, Definition<'b>>,
            visited: &mut HashSet<String>,
            temp_visited: &mut HashSet<String>,
            result: &mut Vec<Definition<'b>>,
        ) {
            if visited.contains(name) {
                return;
            }
            if temp_visited.contains(name) {
                eprintln!("Warning: Circular dependency detected for {}", name);
                return;
            }

            temp_visited.insert(name.to_string());

            if let Some(def) = def_map.get(name) {
                let dependencies = match def {
                    Definition::Struct(s) => get_struct_dependencies(s),
                    Definition::Union(u) => get_union_dependencies(u),
                };

                for dep_name in dependencies {
                    if def_map.contains_key(&dep_name) {
                        visit(&dep_name, def_map, visited, temp_visited, result);
                    }
                }

                result.push(def.clone());
            }

            temp_visited.remove(name);
            visited.insert(name.to_string());
        }

        fn get_struct_dependencies(s: &LirStructDef) -> Vec<String> {
            let mut deps = Vec::new();
            for (_, ty) in &s.fields {
                extract_type_name(ty, &mut deps);
            }
            deps
        }

        fn get_union_dependencies(u: &LirUnionDef) -> Vec<String> {
            let mut deps = Vec::new();
            for (_, ty) in &u.variants {
                extract_type_name(ty, &mut deps);
            }
            deps
        }

        fn extract_type_name(ty: &LirType, deps: &mut Vec<String>) {
            match ty {
                LirType::Struct(n) => {
                    deps.push(n.clone());
                }
                _ => {}
            }
        }

        for def in &defs {
            if !visited.contains(def.name()) {
                visit(
                    def.name(),
                    &def_map,
                    &mut visited,
                    &mut temp_visited,
                    &mut result,
                );
            }
        }

        result
    }

    fn get_func_params(&self, func: &LirFunctionDef) -> Vec<(String, LirType)> {
        func.params.clone()
    }

    fn compile_function(&mut self, func: &LirFunctionDef) {
        let ret_ty = &func.return_type;
        let params = self.get_func_params(func);

        self.target
            .begin_function(&func.name, &params, ret_ty, func.is_variadic);

        for stmt in &func.body {
            self.process_stmt(stmt);
        }

        self.target.end_function();
    }

    // --- Statement Processor ---

    fn process_stmt(&mut self, stmt: &LirStmt) {
        match stmt {
            LirStmt::Let(name, ty, init_opt) => {
                let has_init = init_opt.is_some();

                self.target.stmt_var_decl(name, ty, has_init);

                if let Some(init_expr) = init_opt {
                    self.process_expr(init_expr);
                    self.target.stmt_var_init_end();
                }
            }
            LirStmt::Assign(lhs, rhs) => {
                let is_ptr_assign = matches!(lhs, LirExpr::Deref(_));

                self.process_expr(lhs);

                self.target.stmt_assign_start(is_ptr_assign);
                self.process_expr(rhs);
                self.target.stmt_assign_end();
            }
            LirStmt::ExprStmt(expr) => {
                self.process_expr(expr);
                self.target.stmt_expr_end();
            }
            LirStmt::Return(expr_opt) => {
                self.target.stmt_return_start();
                if let Some(expr) = expr_opt {
                    self.process_expr(expr);
                }
                self.target.stmt_return_end();
            }
            LirStmt::Break => self.target.stmt_break(),
            LirStmt::Continue => self.target.stmt_continue(),

            LirStmt::Block(stmts) => {
                self.target.begin_block();
                for s in stmts {
                    self.process_stmt(s);
                }
                self.target.end_block();
            }

            LirStmt::If {
                cond,
                then_branch,
                else_branch,
            } => {
                self.target.begin_if();
                self.process_expr(cond);
                self.target.begin_if_body();
                for s in then_branch {
                    self.process_stmt(s);
                }

                if !else_branch.is_empty() {
                    self.target.begin_else();
                    for s in else_branch {
                        self.process_stmt(s);
                    }
                }
                self.target.end_if();
            }

            LirStmt::While { cond, body } => {
                self.target.begin_while();
                self.process_expr(cond);
                self.target.begin_while_body();
                for s in body {
                    self.process_stmt(s);
                }
                self.target.end_while();
            }

            LirStmt::Switch {
                expr,
                cases,
                default,
            } => {
                self.target.begin_switch();
                self.process_expr(expr);
                self.target.begin_switch_body();

                for (lit, stmts) in cases {
                    self.target.begin_case(lit);
                    for s in stmts {
                        self.process_stmt(s);
                    }
                    self.target.end_case();
                }

                if !default.is_empty() {
                    self.target.begin_default();
                    for s in default {
                        self.process_stmt(s);
                    }
                    self.target.end_case();
                }

                self.target.end_switch();
            }
        }
    }

    // --- Expression Processor ---

    fn process_expr(&mut self, expr: &LirExpr) {
        match expr {
            LirExpr::Lit(l) => self.target.expr_lit(l),
            LirExpr::Ident(n) => self.target.expr_ident(n),

            LirExpr::Binary(lhs, op, rhs) => {
                self.target.expr_binary_start(*op);
                self.process_expr(lhs);
                self.target.expr_binary_mid(*op);
                self.process_expr(rhs);
                self.target.expr_binary_end();
            }

            LirExpr::Unary(op, operand) => {
                self.target.expr_unary_start(*op);
                self.process_expr(operand);
                self.target.expr_unary_end();
            }

            LirExpr::Call { func_name, args } => {
                self.target.expr_call_start(func_name);
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        self.target.expr_call_arg_sep();
                    }
                    self.process_expr(arg);
                }
                self.target.expr_call_end();
            }

            LirExpr::CallPtr(func_expr, _) => {
                self.target.expr_unary_start(UnaryOp::Not);
                self.target.expr_deref_start();
                self.process_expr(func_expr);
                self.target.expr_deref_end();
            }

            LirExpr::MemberAccess(obj, field) => {
                self.process_expr(obj);
                self.target.expr_member(field, false); // .field
            }

            LirExpr::MemberAccessPtr(obj, field) => {
                self.process_expr(obj);
                self.target.expr_member(field, true); // ->field
            }

            LirExpr::Index(arr, idx) => {
                self.process_expr(arr);
                self.target.expr_index_start();
                self.process_expr(idx);
                self.target.expr_index_end();
            }

            LirExpr::AddrOf(inner) => {
                self.target.expr_addrof_start();
                self.process_expr(inner);
                self.target.expr_addrof_end();
            }

            LirExpr::Deref(inner) => {
                self.target.expr_deref_start();
                self.process_expr(inner);
                self.target.expr_deref_end();
            }

            LirExpr::Cast(inner, ty) => {
                self.target.expr_cast_start(ty);
                self.process_expr(inner);
                self.target.expr_cast_end();
            }

            LirExpr::Is(inner, ty) => {
                self.target.expr_is_start();
                self.process_expr(inner);
                self.target.expr_is_end(ty);
            }

            LirExpr::SizeOf(ty) => {
                self.target.expr_sizeof(ty);
            }

            LirExpr::StructInit {
                struct_name,
                fields,
            } => {
                self.target.expr_struct_init_start(struct_name);

                for (i, (field_name, val_expr)) in fields.iter().enumerate() {
                    if i > 0 {
                        self.target.expr_struct_init_sep();
                    }
                    self.target.expr_struct_init_field_start(field_name);
                    self.process_expr(val_expr);
                }

                self.target.expr_struct_init_end();
            }

            LirExpr::UnionInit {
                union_name,
                variants,
            } => {
                self.target.expr_union_init_start(union_name);

                if let Some((field_name, val_expr)) = variants.first() {
                    self.target.expr_union_init_field_start(field_name);
                    self.process_expr(val_expr);
                }

                self.target.expr_union_init_end();
            }

            LirExpr::Ternary(cond, then_expr, else_expr) => {
                self.process_expr(cond);
                self.target.expr_ternary_mid1();
                self.process_expr(then_expr);
                self.target.expr_ternary_mid2();
                self.process_expr(else_expr);
            }
            LirExpr::ArrayInit(items) => {
                let ty_hint = self.infer_array_type(items);

                self.target.expr_array_init_start(ty_hint.as_ref());

                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        self.target.expr_array_init_sep();
                    }
                    self.process_expr(item);
                }
                self.target.expr_array_init_end();
            }
        }
    }
}
