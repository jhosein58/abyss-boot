use crate::hir::FlatProgram;
use abyss_parser::ast::{BinaryOp, Expr, FunctionBody, FunctionDef, Lit, Stmt, StructDef, Type};
use std::collections::{HashMap, VecDeque};

pub struct TypeChecker {
    concrete_funcs: Vec<FunctionDef>,
    concrete_structs: Vec<StructDef>,
    generic_func_templates: HashMap<String, FunctionDef>,
    generic_struct_templates: HashMap<String, StructDef>,
    monomorphization_cache: HashMap<(String, String), String>,
    reverse_struct_map: HashMap<String, (String, Vec<Type>)>,
    pending_funcs: VecDeque<FunctionDef>,
    scopes: Vec<HashMap<String, Type>>,
    local_func_scopes: Vec<HashMap<String, FunctionDef>>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            concrete_funcs: Vec::new(),
            concrete_structs: Vec::new(),
            generic_func_templates: HashMap::new(),
            generic_struct_templates: HashMap::new(),
            monomorphization_cache: HashMap::new(),
            reverse_struct_map: HashMap::new(),
            pending_funcs: VecDeque::new(),
            scopes: vec![HashMap::new()],
            local_func_scopes: vec![HashMap::new()],
        }
    }

    pub fn check(mut self, program: FlatProgram) -> FlatProgram {
        for mut func in program.functions {
            if !func.generics.is_empty() {
                self.resolve_generics_in_func(&mut func);
                self.generic_func_templates.insert(func.name.clone(), func);
            } else {
                self.pending_funcs.push_back(func);
            }
        }

        for mut s in program.structs {
            if !s.generics.is_empty() {
                self.resolve_generics_in_struct(&mut s);

                self.generic_struct_templates.insert(s.name.clone(), s);
            } else {
                let empty_map = HashMap::new();
                for (_, field_ty) in &mut s.fields {
                    self.substitute_type(field_ty, &empty_map);
                }
                self.concrete_structs.push(s);
            }
        }

        while let Some(mut func) = self.pending_funcs.pop_front() {
            self.check_function(&mut func);
            self.concrete_funcs.push(func);
        }

        let mut new_program = FlatProgram::new();
        new_program.functions = self.concrete_funcs;
        new_program.structs = self.concrete_structs;
        new_program.statics = program.statics;
        new_program
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
        self.local_func_scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
        self.local_func_scopes.pop();
    }
    fn register_local_func(&mut self, name: String, func: FunctionDef) {
        if let Some(scope) = self.local_func_scopes.last_mut() {
            scope.insert(name, func);
        }
    }
    fn get_local_func(&self, name: &str) -> Option<FunctionDef> {
        for scope in self.local_func_scopes.iter().rev() {
            if let Some(func) = scope.get(name) {
                return Some(func.clone());
            }
        }
        None
    }
    fn register_var(&mut self, name: String, ty: Type) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, ty);
        }
    }

    fn get_var_type(&self, name: &str) -> Option<Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty.clone());
            }
        }
        None
    }

    fn resolve_generics_in_func(&self, func: &mut FunctionDef) {
        let generics = func.generics.clone();

        for (_, param_ty) in &mut func.params {
            self.convert_struct_to_generic(param_ty, &generics);
        }

        self.convert_struct_to_generic(&mut func.return_type, &generics);

        if let FunctionBody::UserDefined(stmts) = &mut func.body {
            for stmt in stmts {
                self.resolve_generics_in_stmt(stmt, &generics);
            }
        }
    }

    fn convert_struct_to_generic(&self, ty: &mut Type, generic_names: &[String]) {
        match ty {
            Type::Struct(path, args) => {
                if path.len() == 1 && args.is_empty() {
                    if generic_names.contains(&path[0]) {
                        *ty = Type::Generic(path[0].clone());
                        return;
                    }
                }
                for arg in args {
                    self.convert_struct_to_generic(arg, generic_names);
                }
            }
            Type::Pointer(inner) | Type::Array(inner, _) => {
                self.convert_struct_to_generic(inner, generic_names);
            }
            Type::Function(args, ret, _) => {
                for arg in args {
                    self.convert_struct_to_generic(arg, generic_names);
                }
                self.convert_struct_to_generic(ret, generic_names);
            }
            _ => {}
        }
    }

    fn resolve_generics_in_struct(&self, struct_def: &mut StructDef) {
        let generics = struct_def.generics.clone();
        for (_, field_ty) in &mut struct_def.fields {
            self.convert_struct_to_generic(field_ty, &generics);
        }
    }

    fn resolve_generics_in_stmt(&self, stmt: &mut Stmt, generic_names: &[String]) {
        match stmt {
            Stmt::Let(_, ty_opt, expr_opt) => {
                if let Some(ty) = ty_opt {
                    self.convert_struct_to_generic(ty, generic_names);
                }
                if let Some(expr) = expr_opt {
                    self.resolve_generics_in_expr(expr, generic_names);
                }
            }
            Stmt::Assign(lhs, rhs) => {
                self.resolve_generics_in_expr(lhs, generic_names);
                self.resolve_generics_in_expr(rhs, generic_names);
            }
            Stmt::Expr(expr) | Stmt::Ret(expr) => {
                self.resolve_generics_in_expr(expr, generic_names);
            }
            Stmt::If(cond, then_b, else_b) => {
                self.resolve_generics_in_expr(cond, generic_names);
                self.resolve_generics_in_stmt(then_b, generic_names);
                if let Some(e) = else_b {
                    self.resolve_generics_in_stmt(e, generic_names);
                }
            }
            Stmt::While(cond, body) => {
                self.resolve_generics_in_expr(cond, generic_names);
                self.resolve_generics_in_stmt(body, generic_names);
            }
            Stmt::Block(stmts) => {
                for s in stmts {
                    self.resolve_generics_in_stmt(s, generic_names);
                }
            }
            _ => {}
        }
    }

    fn resolve_generics_in_expr(&self, expr: &mut Expr, generic_names: &[String]) {
        match expr {
            Expr::Cast(inner, ty) => {
                self.resolve_generics_in_expr(inner, generic_names);
                self.convert_struct_to_generic(ty, generic_names);
            }
            Expr::Binary(lhs, _, rhs) => {
                self.resolve_generics_in_expr(lhs, generic_names);
                self.resolve_generics_in_expr(rhs, generic_names);
            }
            Expr::Call(callee, args, generics) => {
                self.resolve_generics_in_expr(callee, generic_names);
                for arg in args {
                    self.resolve_generics_in_expr(arg, generic_names);
                }
                for g in generics {
                    self.convert_struct_to_generic(g, generic_names);
                }
            }
            Expr::StructInit(_, fields, generics) => {
                for (_, val) in fields {
                    self.resolve_generics_in_expr(val, generic_names);
                }
                for g in generics {
                    self.convert_struct_to_generic(g, generic_names);
                }
            }
            Expr::Unary(_, inner)
            | Expr::Deref(inner)
            | Expr::AddrOf(inner)
            | Expr::Member(inner, _) => {
                self.resolve_generics_in_expr(inner, generic_names);
            }
            Expr::SizeOf(ty) => {
                self.convert_struct_to_generic(ty, generic_names);
            }
            Expr::Index(arr, idx) => {
                self.resolve_generics_in_expr(arr, generic_names);
                self.resolve_generics_in_expr(idx, generic_names);
            }
            Expr::Lit(Lit::Array(exprs)) => {
                for e in exprs {
                    self.resolve_generics_in_expr(e, generic_names);
                }
            }
            _ => {}
        }
    }

    fn check_function(&mut self, func: &mut FunctionDef) {
        self.enter_scope();
        for (param_name, param_type) in &func.params {
            self.register_var(param_name.clone(), param_type.clone());
        }
        if let FunctionBody::UserDefined(ref mut stmts) = func.body {
            self.check_stmts(stmts);
        }
        self.exit_scope();
    }

    fn check_stmts(&mut self, stmts: &mut [Stmt]) {
        for stmt in stmts {
            self.check_stmt(stmt);
        }
    }

    fn check_stmt(&mut self, stmt: &mut Stmt) {
        match stmt {
            Stmt::Let(name, ty_opt, expr_opt) => {
                let inferred_ty = if let Some(expr) = expr_opt {
                    let (new_expr, expr_ty) = self.infer_expr(expr.clone());
                    *expr = new_expr;
                    expr_ty
                } else {
                    Type::Void
                };

                let final_ty = match ty_opt {
                    Some(explicit_ty) => {
                        if *explicit_ty != inferred_ty {
                            panic!("Type mismatch in let binding for {}", name);
                        }
                        explicit_ty.clone()
                    }
                    None => {
                        *ty_opt = Some(inferred_ty.clone());
                        inferred_ty
                    }
                };

                self.register_var(name.clone(), final_ty);
            }
            Stmt::Assign(lhs, rhs) => {
                let (new_lhs, _) = self.infer_expr(lhs.clone());
                let (new_rhs, _) = self.infer_expr(rhs.clone());
                *lhs = new_lhs;
                *rhs = new_rhs;
            }
            Stmt::Expr(expr) => {
                let (new_expr, _) = self.infer_expr(expr.clone());
                *stmt = Stmt::Expr(new_expr);
            }
            Stmt::Ret(expr) => {
                let (new_expr, _) = self.infer_expr(expr.clone());
                *stmt = Stmt::Ret(new_expr);
            }
            Stmt::If(cond, then_block, else_block) => {
                let (new_cond, _) = self.infer_expr(cond.clone());
                *cond = new_cond;
                self.check_stmt(then_block);
                if let Some(else_b) = else_block {
                    self.check_stmt(else_b);
                }
            }
            Stmt::While(cond, body) => {
                let (new_cond, _) = self.infer_expr(cond.clone());
                *cond = new_cond;
                self.check_stmt(body);
            }
            Stmt::Block(inner_stmts) => {
                self.enter_scope();
                self.check_stmts(inner_stmts);
                self.exit_scope();
            }
            Stmt::FunctionDef(func_def) => {
                self.register_local_func(func_def.name.clone(), *func_def.clone());

                self.check_function(func_def);
            }
            _ => {}
        }
    }

    fn infer_expr(&mut self, expr: Expr) -> (Expr, Type) {
        match expr {
            Expr::Lit(lit) => self.infer_lit(lit),

            Expr::Ident(path) => {
                let name = path.last().unwrap();
                if let Some(ty) = self.get_var_type(name) {
                    (Expr::Ident(path), ty)
                } else {
                    panic!("Undefined variable: {}", name);
                }
            }

            Expr::Call(callee, args, generics) => {
                self.handle_function_call(*callee, args, generics)
            }

            Expr::Binary(lhs, op, rhs) => {
                let (new_lhs, ty_lhs) = self.infer_expr(*lhs);
                let (new_rhs, _) = self.infer_expr(*rhs);

                match op {
                    BinaryOp::Eq
                    | BinaryOp::Neq
                    | BinaryOp::Lt
                    | BinaryOp::Gt
                    | BinaryOp::Lte
                    | BinaryOp::Gte => (
                        Expr::Binary(Box::new(new_lhs), op, Box::new(new_rhs)),
                        Type::Bool,
                    ),
                    _ => (
                        Expr::Binary(Box::new(new_lhs), op, Box::new(new_rhs)),
                        ty_lhs,
                    ),
                }
            }

            Expr::Cast(inner, target_ty) => {
                let (new_inner, _) = self.infer_expr(*inner);
                (
                    Expr::Cast(Box::new(new_inner), target_ty.clone()),
                    target_ty,
                )
            }

            Expr::SizeOf(ty) => (Expr::SizeOf(ty.clone()), Type::I64),

            Expr::Index(arr, idx) => {
                let (new_arr, arr_ty) = self.infer_expr(*arr);
                let (new_idx, _) = self.infer_expr(*idx);

                let elem_ty = match arr_ty {
                    Type::Array(inner, _) => *inner,
                    Type::Pointer(inner) => *inner,
                    _ => panic!("Cannot index type {:?}", arr_ty),
                };

                (Expr::Index(Box::new(new_arr), Box::new(new_idx)), elem_ty)
            }

            Expr::StructInit(path, fields, generics) => {
                let mut new_fields = Vec::new();
                let mut field_types = Vec::new();
                for (name, val) in fields {
                    let (new_val, ty) = self.infer_expr(val);
                    new_fields.push((name, new_val));
                    field_types.push(ty);
                }

                let struct_name = path.join("__");

                if self.generic_struct_templates.contains_key(&struct_name) {
                    let final_generics: Vec<Type>;
                    if !generics.is_empty() {
                        final_generics = generics;
                    } else {
                        panic!("Implicit struct generics not implemented fully yet");
                    }

                    let mangled_name = self.monomorphize_struct(&struct_name, final_generics);

                    (
                        Expr::StructInit(vec![mangled_name.clone()], new_fields, vec![]),
                        Type::Struct(vec![mangled_name], vec![]),
                    )
                } else {
                    (
                        Expr::StructInit(path, new_fields, generics),
                        Type::Struct(vec![struct_name], vec![]),
                    )
                }
            }

            Expr::Member(inner, field_name) => {
                let (new_inner, mut inner_ty) = self.infer_expr(*inner);

                let mut current_expr = new_inner;

                while let Type::Pointer(pointed_to) = inner_ty.clone() {
                    inner_ty = *pointed_to;
                    current_expr = Expr::Deref(Box::new(current_expr));
                }

                if let Type::Struct(path, _) = inner_ty {
                    let struct_name = path.last().unwrap();
                    let def = self
                        .concrete_structs
                        .iter()
                        .find(|s| &s.name == struct_name)
                        .expect("Struct definition not found");

                    let (_, field_ty) = def
                        .fields
                        .iter()
                        .find(|(n, _)| n == &field_name)
                        .expect("Field not found");

                    (
                        Expr::Member(Box::new(current_expr), field_name),
                        field_ty.clone(),
                    )
                } else {
                    panic!("Accessing member of non-struct type");
                }
            }

            Expr::MethodCall(receiver, method_name, args, generics) => {
                let static_struct_name = if let Expr::Ident(ref path) = *receiver {
                    let potential_struct_name = path.join("__");
                    let var_name = path.last().unwrap();

                    if self.get_var_type(var_name).is_none() {
                        if self
                            .generic_struct_templates
                            .contains_key(&potential_struct_name)
                            || self
                                .concrete_structs
                                .iter()
                                .any(|s| s.name == potential_struct_name)
                            || self.reverse_struct_map.contains_key(&potential_struct_name)
                        {
                            Some(potential_struct_name)
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                };

                if let Some(struct_name) = static_struct_name {
                    let func_mangled_name = format!("{}__{}", struct_name, method_name);

                    return self.handle_function_call(
                        Expr::Ident(vec![func_mangled_name]),
                        args,
                        generics,
                    );
                }

                let (new_receiver, mut receiver_ty) = self.infer_expr(*receiver);
                let mut base_receiver_expr = new_receiver;

                while let Type::Pointer(sub) = receiver_ty.clone() {
                    receiver_ty = *sub;
                    base_receiver_expr = Expr::Deref(Box::new(base_receiver_expr));
                }

                if let Type::Struct(path, struct_generics) = &receiver_ty {
                    let current_struct_name = path.last().unwrap();

                    let (base_struct_name, base_generics) = if let Some((base, stored_generics)) =
                        self.reverse_struct_map.get(current_struct_name)
                    {
                        (base.clone(), stored_generics.clone())
                    } else {
                        (current_struct_name.clone(), struct_generics.clone())
                    };

                    let func_mangled_name = format!("{}__{}", base_struct_name, method_name);

                    let mut combined_generics = Vec::new();
                    combined_generics.extend(base_generics);
                    combined_generics.extend(generics);

                    let mut final_args = Vec::new();
                    let mut final_receiver = base_receiver_expr;

                    let mut should_pass_ref = false;

                    let check_first_param = |func_def: &FunctionDef| -> bool {
                        if let Some((_, first_param_ty)) = func_def.params.first() {
                            matches!(first_param_ty, Type::Pointer(_))
                        } else {
                            false
                        }
                    };

                    if let Some(template) = self.generic_func_templates.get(&func_mangled_name) {
                        if check_first_param(template) {
                            should_pass_ref = true;
                        }
                    } else if let Some(func) = self
                        .concrete_funcs
                        .iter()
                        .find(|f| f.name == func_mangled_name)
                    {
                        if check_first_param(func) {
                            should_pass_ref = true;
                        }
                    } else if let Some(func) = self
                        .pending_funcs
                        .iter()
                        .find(|f| f.name == func_mangled_name)
                    {
                        if check_first_param(func) {
                            should_pass_ref = true;
                        }
                    }

                    if should_pass_ref {
                        final_receiver = Expr::AddrOf(Box::new(final_receiver));
                        final_args.push(final_receiver);
                    } else {
                        final_args.push(final_receiver);
                    }

                    final_args.extend(args);

                    return self.handle_function_call(
                        Expr::Ident(vec![func_mangled_name]),
                        final_args,
                        combined_generics,
                    );
                } else {
                    panic!(
                        "Method call '{}' on non-struct type {:?}",
                        method_name, receiver_ty
                    );
                }
            }

            _ => (expr, Type::Void),
        }
    }

    fn infer_lit(&mut self, lit: Lit) -> (Expr, Type) {
        match lit {
            Lit::Int(_) => (Expr::Lit(lit), Type::I64),
            Lit::Float(_) => (Expr::Lit(lit), Type::F64),
            Lit::Bool(_) => (Expr::Lit(lit), Type::Bool),
            Lit::Str(ref s) => {
                let len = s.len() - 1;
                (Expr::Lit(lit), Type::Array(Box::new(Type::U8), len))
            }

            Lit::Array(exprs) => {
                if exprs.is_empty() {
                    return (
                        Expr::Lit(Lit::Array(exprs)),
                        Type::Array(Box::new(Type::Void), 0),
                    );
                }

                let mut new_exprs = Vec::new();
                let mut first_ty = None;

                for expr in exprs {
                    let (new_expr, ty) = self.infer_expr(expr);
                    new_exprs.push(new_expr);

                    if first_ty.is_none() {
                        first_ty = Some(ty);
                    } else if first_ty.as_ref() != Some(&ty) {
                        panic!(
                            "Array elements mismatch: expected {:?}, found {:?}",
                            first_ty, ty
                        );
                    }
                }

                let len = new_exprs.len();
                let elem_ty = first_ty.unwrap();

                (
                    Expr::Lit(Lit::Array(new_exprs)),
                    Type::Array(Box::new(elem_ty), len),
                )
            }
            _ => (Expr::Lit(lit), Type::Void),
        }
    }
    fn handle_function_call(
        &mut self,
        callee: Expr,
        args: Vec<Expr>,
        explicit_generics: Vec<Type>,
    ) -> (Expr, Type) {
        let func_name = match &callee {
            Expr::Ident(path) => path.join("__"),
            _ => panic!("Complex callee not supported yet"),
        };

        let mut typed_args = Vec::new();
        let mut arg_types = Vec::new();
        for arg in args {
            let (new_arg, ty) = self.infer_expr(arg);
            typed_args.push(new_arg);
            arg_types.push(ty);
        }

        if let Some(func) = self.get_local_func(&func_name) {
            if func.params.len() != typed_args.len() {
                panic!(
                    "Argument count mismatch for local function '{}': expected {}, found {}",
                    func_name,
                    func.params.len(),
                    typed_args.len()
                );
            }

            for (i, ((_, param_ty), arg_ty)) in func.params.iter().zip(arg_types.iter()).enumerate()
            {
                if param_ty != arg_ty {
                    panic!(
                        "Type mismatch for argument {} in local function '{}': expected {:?}, found {:?}",
                        i + 1,
                        func_name,
                        param_ty,
                        arg_ty
                    );
                }
            }

            return (
                Expr::Call(Box::new(callee), typed_args, explicit_generics),
                func.return_type.clone(),
            );
        }

        if let Some(template) = self.generic_func_templates.get(&func_name).cloned() {
            let mut final_generics: Vec<Type>;
            if !explicit_generics.is_empty() {
                if explicit_generics.len() != template.generics.len() {
                    panic!("Generic count mismatch for function '{}'", func_name);
                }
                final_generics = explicit_generics;
            } else {
                final_generics =
                    self.infer_generics_from_args(&template.generics, &template.params, &arg_types);
            }

            let empty_map = HashMap::new();
            for g in &mut final_generics {
                self.substitute_type(g, &empty_map);
            }

            let generics_key = format!("{:?}", final_generics);
            let cache_key = (func_name.clone(), generics_key);

            let mangled_name = if let Some(name) = self.monomorphization_cache.get(&cache_key) {
                name.clone()
            } else {
                let new_name = format!("{}_{}", func_name, self.monomorphization_cache.len());
                let mut new_func = template.clone();
                new_func.name = new_name.clone();
                new_func.generics.clear();

                self.monomorphization_cache
                    .insert(cache_key, new_name.clone());

                self.replace_generics_in_func(&mut new_func, &template.generics, &final_generics);
                self.pending_funcs.push_back(new_func);

                new_name
            };

            let mut ret_ty = template.return_type.clone();
            let mut map = HashMap::new();
            for (name, ty) in template.generics.iter().zip(final_generics.iter()) {
                map.insert(name.clone(), ty.clone());
            }
            self.substitute_type(&mut ret_ty, &map);

            return (
                Expr::Call(
                    Box::new(Expr::Ident(vec![mangled_name])),
                    typed_args,
                    vec![],
                ),
                ret_ty,
            );
        }

        if let Some(func) = self.concrete_funcs.iter().find(|f| f.name == func_name) {
            return (
                Expr::Call(Box::new(callee), typed_args, explicit_generics),
                func.return_type.clone(),
            );
        }

        if let Some(func) = self.pending_funcs.iter().find(|f| f.name == func_name) {
            return (
                Expr::Call(Box::new(callee), typed_args, explicit_generics),
                func.return_type.clone(),
            );
        }

        panic!(
            "Undefined function: '{}'. Did you mean to use a full path (e.g. std::str::new)?",
            func_name
        );
    }

    fn replace_generics_in_func(
        &mut self,
        func: &mut FunctionDef,
        generic_names: &[String],
        concrete_types: &[Type],
    ) {
        let mut map = HashMap::new();
        for (name, ty) in generic_names.iter().zip(concrete_types.iter()) {
            map.insert(name.clone(), ty.clone());
        }

        for (_, ty) in &mut func.params {
            self.substitute_type(ty, &map);
        }
        self.substitute_type(&mut func.return_type, &map);

        if let FunctionBody::UserDefined(stmts) = &mut func.body {
            for stmt in stmts {
                self.substitute_stmt(stmt, &map);
            }
        }
    }
    fn monomorphize_struct(&mut self, template_name: &str, concrete_generics: Vec<Type>) -> String {
        let generics_key = format!("{:?}", concrete_generics);
        let cache_key = (template_name.to_string(), generics_key);

        if let Some(name) = self.monomorphization_cache.get(&cache_key) {
            return name.clone();
        }

        let template = self
            .generic_struct_templates
            .get(template_name)
            .expect(&format!("Template not found: {}", template_name))
            .clone();
        let new_name = format!("{}_{}", template_name, self.monomorphization_cache.len());

        self.monomorphization_cache
            .insert(cache_key.clone(), new_name.clone());

        self.reverse_struct_map.insert(
            new_name.clone(),
            (template_name.to_string(), concrete_generics.clone()),
        );

        let mut new_struct = template.clone();
        new_struct.name = new_name.clone();
        new_struct.generics.clear();

        let mut map = HashMap::new();
        for (name, ty) in template.generics.iter().zip(concrete_generics.iter()) {
            map.insert(name.clone(), ty.clone());
        }

        for (_, field_ty) in &mut new_struct.fields {
            self.substitute_type_helper(field_ty, &map);
        }

        self.concrete_structs.push(new_struct);

        new_name
    }

    fn substitute_type_helper(&mut self, ty: &mut Type, map: &HashMap<String, Type>) {
        self.substitute_type(ty, map);
    }

    fn substitute_type(&mut self, ty: &mut Type, map: &HashMap<String, Type>) {
        match ty {
            Type::Generic(name) => {
                if let Some(concrete) = map.get(name) {
                    *ty = concrete.clone();
                    self.substitute_type(ty, map);
                }
            }
            Type::Pointer(inner) | Type::Array(inner, _) => {
                self.substitute_type(inner, map);
            }
            Type::Function(args, ret, _) => {
                for arg in args {
                    self.substitute_type(arg, map);
                }
                self.substitute_type(ret, map);
            }
            Type::Struct(path, generics) => {
                for g in generics.iter_mut() {
                    self.substitute_type(g, map);
                }

                if !generics.is_empty() {
                    let struct_name = path.join("__");

                    if self.generic_struct_templates.contains_key(&struct_name) {
                        let mangled_name = self.monomorphize_struct(&struct_name, generics.clone());

                        *path = vec![mangled_name];
                        generics.clear();
                    }
                }
            }
            _ => {}
        }
    }

    fn substitute_stmt(&mut self, stmt: &mut Stmt, map: &HashMap<String, Type>) {
        match stmt {
            Stmt::Let(_, ty_opt, expr_opt) => {
                if let Some(ty) = ty_opt {
                    self.substitute_type(ty, map);
                }
                if let Some(expr) = expr_opt {
                    self.substitute_expr(expr, map);
                }
            }
            Stmt::Assign(lhs, rhs) => {
                self.substitute_expr(lhs, map);
                self.substitute_expr(rhs, map);
            }
            Stmt::Expr(expr) | Stmt::Ret(expr) => {
                self.substitute_expr(expr, map);
            }
            Stmt::If(cond, then_block, else_block) => {
                self.substitute_expr(cond, map);
                self.substitute_stmt(then_block, map);
                if let Some(e) = else_block {
                    self.substitute_stmt(e, map);
                }
            }
            Stmt::While(cond, body) => {
                self.substitute_expr(cond, map);
                self.substitute_stmt(body, map);
            }
            Stmt::Block(stmts) => {
                for s in stmts {
                    self.substitute_stmt(s, map);
                }
            }
            _ => {}
        }
    }

    fn substitute_expr(&mut self, expr: &mut Expr, map: &HashMap<String, Type>) {
        match expr {
            Expr::Call(callee, args, generics) => {
                self.substitute_expr(callee, map);
                for arg in args {
                    self.substitute_expr(arg, map);
                }
                for g in generics {
                    self.substitute_type(g, map);
                }
            }
            Expr::Binary(l, _, r) => {
                self.substitute_expr(l, map);
                self.substitute_expr(r, map);
            }
            Expr::Unary(_, inner)
            | Expr::Deref(inner)
            | Expr::AddrOf(inner)
            | Expr::Member(inner, _) => {
                self.substitute_expr(inner, map);
            }
            Expr::Cast(inner, ty) => {
                self.substitute_expr(inner, map);
                self.substitute_type(ty, map);
            }
            Expr::StructInit(_, fields, generics) => {
                for (_, e) in fields {
                    self.substitute_expr(e, map);
                }
                for g in generics {
                    self.substitute_type(g, map);
                }
            }
            Expr::SizeOf(ty) => {
                self.substitute_type(ty, map);
            }
            Expr::Lit(Lit::Array(exprs)) => {
                for e in exprs {
                    self.substitute_expr(e, map);
                }
            }
            _ => {}
        }
    }

    fn infer_generics_from_args(
        &self,
        generic_names: &[String],
        param_defs: &[(String, Type)],
        arg_types: &[Type],
    ) -> Vec<Type> {
        let mut resolved_map: HashMap<String, Type> = HashMap::new();

        for ((_, param_type), arg_type) in param_defs.iter().zip(arg_types.iter()) {
            self.match_types(param_type, arg_type, &mut resolved_map);
        }

        let mut result = Vec::new();
        for name in generic_names {
            match resolved_map.get(name) {
                Some(ty) => result.push(ty.clone()),
                None => panic!("Could not infer generic type '{}'", name),
            }
        }
        result
    }

    fn match_types(&self, param_ty: &Type, arg_ty: &Type, map: &mut HashMap<String, Type>) {
        match (param_ty, arg_ty) {
            (Type::Generic(name), concrete) => {
                if let Some(existing) = map.get(name) {
                    if existing != concrete {}
                } else {
                    map.insert(name.clone(), concrete.clone());
                }
            }
            (Type::Pointer(p_inner), Type::Pointer(a_inner)) => {
                self.match_types(p_inner, a_inner, map);
            }
            (Type::Array(p_inner, _), Type::Array(a_inner, _)) => {
                self.match_types(p_inner, a_inner, map);
            }

            (Type::Struct(p_path, p_generics), Type::Struct(a_path, a_generics)) => {
                if p_generics.len() == a_generics.len() {
                    for (p, a) in p_generics.iter().zip(a_generics.iter()) {
                        self.match_types(p, a, map);
                    }
                } else {
                    let a_name = a_path.last().unwrap();
                    if let Some((base_name, base_generics)) = self.reverse_struct_map.get(a_name) {
                        let p_name = p_path.last().unwrap();
                        if p_path.join("__") == *base_name || p_name == base_name {
                            for (p_gen, concrete_gen) in p_generics.iter().zip(base_generics.iter())
                            {
                                self.match_types(p_gen, concrete_gen, map);
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }
}
