use crate::hir::FlatProgram;
use abyss_parser::ast::{
    BinaryOp, Expr, FunctionBody, FunctionDef, Lit, Stmt, StructDef, Type, UnionDef,
};
use std::collections::{HashMap, VecDeque};

pub struct TypeChecker {
    concrete_funcs: Vec<FunctionDef>,
    concrete_structs: Vec<StructDef>,
    concrete_unions: Vec<UnionDef>,
    generic_func_templates: HashMap<String, FunctionDef>,
    generic_struct_templates: HashMap<String, StructDef>,
    monomorphization_cache: HashMap<(String, String), String>,
    reverse_struct_map: HashMap<String, (String, Vec<Type>)>,
    pending_funcs: VecDeque<FunctionDef>,
    scopes: Vec<HashMap<String, Type>>,
    local_func_scopes: Vec<HashMap<String, FunctionDef>>,
    used_type_tags: HashMap<String, i64>,
    union_struct_defs: Vec<StructDef>,
    variant_cache: HashMap<String, Vec<Type>>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            concrete_funcs: Vec::new(),
            concrete_structs: Vec::new(),
            concrete_unions: Vec::new(),
            generic_func_templates: HashMap::new(),
            generic_struct_templates: HashMap::new(),
            monomorphization_cache: HashMap::new(),
            reverse_struct_map: HashMap::new(),
            pending_funcs: VecDeque::new(),
            scopes: vec![HashMap::new()],
            local_func_scopes: vec![HashMap::new()],
            used_type_tags: HashMap::new(),
            union_struct_defs: Vec::new(),
            variant_cache: HashMap::new(),
        }
    }

    pub fn get_type_tag(&mut self, ty: &Type) -> i64 {
        let (name, id) = match ty {
            Type::U8 => ("TYPE_TAG_U8".to_string(), 1),
            Type::U16 => ("TYPE_TAG_U16".to_string(), 2),
            Type::U32 => ("TYPE_TAG_U32".to_string(), 3),
            Type::U64 => ("TYPE_TAG_U64".to_string(), 4),
            Type::Usize => ("TYPE_TAG_USIZE".to_string(), 5),
            Type::I8 => ("TYPE_TAG_I8".to_string(), 6),
            Type::I16 => ("TYPE_TAG_I16".to_string(), 7),
            Type::I32 => ("TYPE_TAG_I32".to_string(), 8),
            Type::I64 => ("TYPE_TAG_I64".to_string(), 9),
            Type::Isize => ("TYPE_TAG_ISIZE".to_string(), 10),
            Type::F32 => ("TYPE_TAG_F32".to_string(), 11),
            Type::F64 => ("TYPE_TAG_F64".to_string(), 12),
            Type::Bool => ("TYPE_TAG_BOOL".to_string(), 13),
            Type::Char => ("TYPE_TAG_CHAR".to_string(), 14),
            Type::Array(inner, _) if **inner == Type::U8 => ("TYPE_TAG_U8".to_string(), 1),
            _ => {
                let s = format!("{:?}", ty);
                let mut hash: i64 = 0;
                for c in s.bytes() {
                    hash = hash.wrapping_add(c as i64);
                }
                (format!("TYPE_TAG_{}", hash), hash)
            }
        };
        self.used_type_tags.insert(name, id);
        id
    }

    fn get_union_name(&mut self, types: &[Type]) -> String {
        let mut types_str = types.iter().map(|t| t.get_name()).collect::<Vec<_>>();
        types_str.sort();
        types_str.join("_")
    }

    fn get_or_create_union_struct(&mut self, types: &[Type]) -> String {
        let mut sorted_types = types.to_vec();
        sorted_types.sort_by_key(|t| t.get_name());

        let id = self.get_union_name(&sorted_types);

        let struct_name = format!("__Union_{}", id);
        let inner_struct_name = format!("__UnionInner_{}", id);

        if !self.variant_cache.contains_key(&struct_name) {
            self.variant_cache
                .insert(struct_name.clone(), sorted_types.clone());
        }

        if !self
            .concrete_unions
            .iter()
            .any(|u| u.name == inner_struct_name)
        {
            let mut inner_fields = Vec::new();
            for (i, t) in sorted_types.iter().enumerate() {
                inner_fields.push((format!("variant_{}", i), t.clone()));
            }

            let inner_def = UnionDef {
                is_pub: false,
                name: inner_struct_name.clone(),
                fields: inner_fields,
            };
            self.concrete_unions.push(inner_def);

            let fields = vec![
                ("tag".to_string(), Type::I64),
                (
                    "data".to_string(),
                    Type::Struct(vec![inner_struct_name.clone()], vec![]),
                ),
            ];

            let struct_def = StructDef {
                is_pub: false,
                name: struct_name.clone(),
                generics: vec![],
                fields,
            };
            self.union_struct_defs.push(struct_def);
        }

        struct_name
    }

    fn wrap_expr_for_union(
        &mut self,
        mut expr: Expr,
        expr_ty: Type,
        variants: &[Type],
        target_struct_name: String,
    ) -> (Expr, Type) {
        if !variants.contains(&expr_ty) {
            for variant in variants {
                let is_int_conv = self.is_integer(variant) && self.is_integer(&expr_ty);
                let is_float_conv = self.is_float(variant) && self.is_float(&expr_ty);

                if is_int_conv || is_float_conv {
                    expr = Expr::Cast(Box::new(expr), variant.clone());
                    break;
                }
            }
        }

        let (_, final_rhs_ty) = self.infer_expr(expr.clone());
        let tag_val = self.get_type_tag(&final_rhs_ty);

        let mut sorted_variants = variants.to_vec();
        sorted_variants.sort_by_key(|t| t.get_name());

        if let Some(variant_index) = sorted_variants.iter().position(|t| t == &final_rhs_ty) {
            let inner_struct_name = target_struct_name.replace("__Union_", "__UnionInner_");

            let inner_init = Expr::UnionInit(
                vec![inner_struct_name.clone()],
                vec![(format!("variant_{}", variant_index), expr)],
            );

            let wrapper_init = Expr::StructInit(
                vec![target_struct_name.clone()],
                vec![
                    ("tag".to_string(), Expr::Lit(Lit::Int(tag_val))),
                    ("data".to_string(), inner_init),
                ],
                vec![],
            );

            let wrapper_type = Type::Struct(vec![target_struct_name], vec![]);
            (wrapper_init, wrapper_type)
        } else {
            panic!(
                "Type mismatch: Cannot assign {:?} to Union Wrapper {:?} (Variants: {:?})",
                final_rhs_ty, target_struct_name, variants
            );
        }
    }

    fn are_types_compatible(&self, target: &Type, source: &Type) -> bool {
        if target == source {
            return true;
        }

        match (target, source) {
            (Type::Pointer(_), Type::Pointer(inner)) if **inner == Type::Void => true,

            (Type::Pointer(inner), Type::Pointer(_)) if **inner == Type::Void => true,

            (Type::Union(variants), src_ty) => variants.contains(src_ty),

            _ => false,
        }
    }
    fn is_integer(&self, t: &Type) -> bool {
        matches!(
            t,
            Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::Isize
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::U64
                | Type::Usize
                | Type::Char
        )
    }

    fn is_float(&self, t: &Type) -> bool {
        matches!(t, Type::F32 | Type::F64)
    }
    pub fn check(mut self, mut program: FlatProgram) -> FlatProgram {
        for mut s in program.structs {
            if !s.generics.is_empty() {
                self.resolve_generics_in_struct(&mut s);
                self.generic_struct_templates.insert(s.name.clone(), s);
            } else {
                let empty_map = HashMap::new();
                for (_, field_ty) in &mut s.fields {
                    self.substitute_type(field_ty, &empty_map);

                    if let Type::Union(variants) = field_ty {
                        let struct_name = self.get_or_create_union_struct(variants);
                        *field_ty = Type::Struct(vec![struct_name], vec![]);
                    }
                }
                self.concrete_structs.push(s);
            }
        }

        for mut func in program.functions {
            if !func.generics.is_empty() {
                self.resolve_generics_in_func(&mut func);
                self.generic_func_templates.insert(func.name.clone(), func);
            } else {
                self.pending_funcs.push_back(func);
            }
        }

        for static_def in &mut program.statics {
            if let Type::Struct(path, generics) = &mut static_def.ty {
                if !generics.is_empty() {
                    let struct_name = path.join("__");

                    if self.generic_struct_templates.contains_key(&struct_name) {
                        let concrete_name =
                            self.monomorphize_struct(&struct_name, generics.clone());

                        static_def.ty = Type::Struct(vec![concrete_name], vec![]);
                    }
                }
            }

            self.register_var(static_def.name.clone(), static_def.ty.clone());

            let (new_expr, _expr_ty) = self.infer_expr(static_def.value.clone());
            static_def.value = new_expr;
        }

        while let Some(mut func) = self.pending_funcs.pop_front() {
            let empty_map: HashMap<String, Type> = HashMap::new();

            for (_, param_ty) in &mut func.params {
                self.substitute_type(param_ty, &empty_map);
            }

            self.substitute_type(&mut func.return_type, &empty_map);

            if let FunctionBody::UserDefined(stmts) = &mut func.body {
                for stmt in stmts {
                    self.substitute_stmt(stmt, &empty_map);
                }
            }

            self.check_function(&mut func);
            self.concrete_funcs.push(func);
        }

        let mut new_program = FlatProgram::new();
        new_program.functions = self.concrete_funcs;
        new_program.structs = self.concrete_structs;
        new_program.statics = program.statics;
        new_program.unions = self.concrete_unions;
        new_program.union_struct_defs = self.union_struct_defs;
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

            Type::Union(variants) => {
                for variant in variants {
                    self.convert_struct_to_generic(variant, generic_names);
                }
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

        for (param_name, param_type) in &mut func.params {
            if let Type::Union(variants) = param_type {
                let struct_name = self.get_or_create_union_struct(variants);

                *param_type = Type::Struct(vec![struct_name], vec![]);
            }

            self.register_var(param_name.clone(), param_type.clone());
        }

        if let Type::Union(variants) = &func.return_type {
            let struct_name = self.get_or_create_union_struct(variants);
            func.return_type = Type::Struct(vec![struct_name], vec![]);
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
                if let Some(expr) = expr_opt {
                    let (mut new_expr, mut expr_ty) = self.infer_expr(expr.clone());

                    if let Some(Type::Union(variants)) = ty_opt {
                        if !variants.contains(&expr_ty) {
                            for variant in variants {
                                let is_int_conv =
                                    self.is_integer(variant) && self.is_integer(&expr_ty);
                                let is_float_conv =
                                    self.is_float(variant) && self.is_float(&expr_ty);

                                if is_int_conv || is_float_conv {
                                    new_expr = Expr::Cast(Box::new(new_expr), variant.clone());
                                    expr_ty = variant.clone();
                                    break;
                                }
                            }
                        }
                    }

                    let needs_wrapping = if let Some(Type::Union(variants)) = ty_opt {
                        variants.contains(&expr_ty)
                    } else {
                        false
                    };

                    if needs_wrapping {
                        if let Some(Type::Union(variants)) = ty_opt {
                            let struct_name = self.get_or_create_union_struct(variants);
                            let inner_struct_name =
                                struct_name.replace("__Union_", "__UnionInner_");

                            let tag_val = self.get_type_tag(&expr_ty);

                            let mut sorted_variants = variants.clone();
                            sorted_variants.sort_by_key(|t| t.get_name());

                            let variant_index =
                                sorted_variants.iter().position(|t| t == &expr_ty).unwrap();

                            let inner_init = Expr::UnionInit(
                                vec![inner_struct_name],
                                vec![(format!("variant_{}", variant_index), new_expr)],
                            );

                            new_expr = Expr::StructInit(
                                vec![struct_name.clone()],
                                vec![
                                    ("tag".to_string(), Expr::Lit(Lit::Int(tag_val))),
                                    ("data".to_string(), inner_init),
                                ],
                                vec![],
                            );

                            let concrete_struct_type = Type::Struct(vec![struct_name], vec![]);
                            *ty_opt = Some(concrete_struct_type.clone());

                            expr_ty = concrete_struct_type;
                        }
                    }

                    *expr = new_expr;

                    match ty_opt {
                        Some(explicit_ty) => {
                            let mut types_match = self.are_types_compatible(explicit_ty, &expr_ty);

                            if !types_match {
                                if let Type::Struct(explicit_path, explicit_generics) = explicit_ty
                                {
                                    if let Type::Struct(expr_path, _) = &expr_ty {
                                        if let Some(concrete_name) = expr_path.last() {
                                            if let Some((base_name, stored_generics)) =
                                                self.reverse_struct_map.get(concrete_name)
                                            {
                                                let explicit_name_str = explicit_path.join("__");

                                                if &explicit_name_str == base_name
                                                    && explicit_generics == stored_generics
                                                {
                                                    *explicit_ty = expr_ty.clone();
                                                    types_match = true;
                                                }
                                            }
                                        }
                                    }
                                }
                            }

                            if !types_match {
                                let is_int_conversion =
                                    self.is_integer(explicit_ty) && self.is_integer(&expr_ty);
                                let is_float_conversion =
                                    self.is_float(explicit_ty) && self.is_float(&expr_ty);

                                if is_int_conversion || is_float_conversion {
                                    *expr = Expr::Cast(Box::new(expr.clone()), explicit_ty.clone());
                                } else {
                                    panic!(
                                        "Type mismatch in let binding for '{}': expected {:?}, found {:?}",
                                        name, explicit_ty, expr_ty
                                    );
                                }
                            }
                        }
                        None => {
                            *ty_opt = Some(expr_ty.clone());
                        }
                    };

                    let final_ty = ty_opt.as_ref().unwrap().clone();
                    self.register_var(name.clone(), final_ty);
                } else {
                    match ty_opt {
                        Some(Type::Union(variants)) => {
                            let struct_name = self.get_or_create_union_struct(variants);
                            let concrete_ty = Type::Struct(vec![struct_name], vec![]);
                            self.register_var(name.clone(), concrete_ty.clone());
                            *ty_opt = Some(concrete_ty);
                        }
                        Some(explicit_ty) => {
                            self.register_var(name.clone(), explicit_ty.clone());
                        }
                        None => {
                            panic!(
                                "Type annotation required for uninitialized variable '{}'",
                                name
                            );
                        }
                    }
                }
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
            Expr::Binary(lhs, BinaryOp::Assign, rhs) => {
                let (new_lhs, lhs_ty) = self.infer_expr(*lhs);
                let (mut new_rhs, rhs_ty) = self.infer_expr(*rhs);

                if let Type::Struct(names, _) = &lhs_ty {
                    let struct_name = &names[0];
                    if struct_name.starts_with("__Union_") && !struct_name.contains("__UnionInner_")
                    {
                        if &rhs_ty != &lhs_ty {
                            if let Some(variants) = self.variant_cache.get(struct_name).cloned() {
                                let (wrapped_expr, _) = self.wrap_expr_for_union(
                                    new_rhs,
                                    rhs_ty.clone(),
                                    &variants,
                                    struct_name.clone(),
                                );
                                new_rhs = wrapped_expr;
                            }
                        }
                    }
                }

                if let Type::Union(variants) = &lhs_ty {
                    if variants.contains(&rhs_ty) {
                        let struct_name = self.get_or_create_union_struct(variants);
                        let (wrapped_expr, _) =
                            self.wrap_expr_for_union(new_rhs, rhs_ty, variants, struct_name);
                        new_rhs = wrapped_expr;
                    }
                }

                (
                    Expr::Binary(Box::new(new_lhs), BinaryOp::Assign, Box::new(new_rhs)),
                    lhs_ty,
                )
            }

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
            Expr::Is(inner, check_ty) => {
                let (new_inner, inner_ty) = self.infer_expr(*inner);

                if let Type::Union(variants) = &inner_ty {
                    if !variants.contains(&check_ty) {
                        return (Expr::Lit(Lit::Bool(false)), Type::Bool);
                    }
                    let tag_access = Expr::Member(Box::new(new_inner), "tag".to_string());
                    let target_tag = self.get_type_tag(&check_ty);
                    let comparison = Expr::Binary(
                        Box::new(tag_access),
                        BinaryOp::Eq,
                        Box::new(Expr::Lit(Lit::Int(target_tag))),
                    );
                    return (comparison, Type::Bool);
                }

                if let Type::Struct(path, _) = &inner_ty {
                    if let Some(struct_name) = path.last() {
                        if struct_name.starts_with("__Union_") {
                            let inner_union_name = struct_name.replace("__Union_", "__UnionInner_");

                            let union_contains_type = self
                                .concrete_unions
                                .iter()
                                .find(|u| u.name == inner_union_name)
                                .map(|u| u.fields.iter().any(|(_, f_ty)| f_ty == &check_ty))
                                .unwrap_or(false);

                            if union_contains_type {
                                let tag_access =
                                    Expr::Member(Box::new(new_inner), "tag".to_string());
                                let target_tag = self.get_type_tag(&check_ty);
                                let comparison = Expr::Binary(
                                    Box::new(tag_access),
                                    BinaryOp::Eq,
                                    Box::new(Expr::Lit(Lit::Int(target_tag))),
                                );
                                return (comparison, Type::Bool);
                            } else {
                                return (Expr::Lit(Lit::Bool(false)), Type::Bool);
                            }
                        }
                    }
                }

                let result = inner_ty == check_ty;
                (Expr::Lit(Lit::Bool(result)), Type::Bool)
            }

            Expr::Cast(inner, target_ty) => {
                let (new_inner, inner_ty) = self.infer_expr(*inner);

                if let Type::Union(variants) = &target_ty {
                    let is_variant = variants
                        .iter()
                        .any(|v| self.are_types_compatible(v, &inner_ty));

                    if is_variant {
                        let struct_name = self.get_or_create_union_struct(variants);

                        let (wrapped_expr, wrapped_ty) =
                            self.wrap_expr_for_union(new_inner, inner_ty, variants, struct_name);
                        return (wrapped_expr, wrapped_ty);
                    }
                }

                if let Type::Union(variants) = &inner_ty {
                    if variants.contains(&target_ty) {
                        let data_access = Expr::Member(Box::new(new_inner), "data".to_string());
                        let mut sorted_variants = variants.clone();
                        sorted_variants.sort_by_key(|t| t.get_name());

                        let variant_index = sorted_variants
                            .iter()
                            .position(|t| t == &target_ty)
                            .expect("Target type not in union");

                        return (
                            Expr::Member(
                                Box::new(data_access),
                                format!("variant_{}", variant_index),
                            ),
                            target_ty.clone(),
                        );
                    }
                }

                if let Type::Struct(path, _) = &inner_ty {
                    if let Some(struct_name) = path.last() {
                        if struct_name.starts_with("__Union_") {
                            let inner_union_name = struct_name.replace("__Union_", "__UnionInner_");

                            if let Some(union_def) = self
                                .concrete_unions
                                .iter()
                                .find(|u| u.name == inner_union_name)
                            {
                                if let Some((field_name, _)) =
                                    union_def.fields.iter().find(|(_, f_ty)| f_ty == &target_ty)
                                {
                                    let data_access =
                                        Expr::Member(Box::new(new_inner), "data".to_string());

                                    return (
                                        Expr::Member(Box::new(data_access), field_name.clone()),
                                        target_ty.clone(),
                                    );
                                }
                            }
                        }
                    }
                }

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
                let mut resolved_fields = Vec::new();
                for (name, val) in fields {
                    let (new_val, ty) = self.infer_expr(val);
                    resolved_fields.push((name, new_val, ty));
                }

                let struct_name = path.join("__");
                let final_struct_name;

                if self.generic_struct_templates.contains_key(&struct_name) {
                    let final_generics: Vec<Type>;
                    if !generics.is_empty() {
                        final_generics = generics;
                    } else {
                        panic!(
                            "Implicit struct generics logic needed here or explicit generics required"
                        );
                    }
                    final_struct_name = self.monomorphize_struct(&struct_name, final_generics);
                } else {
                    final_struct_name = struct_name;
                }

                let target_def = self
                    .concrete_structs
                    .iter()
                    .find(|s| s.name == final_struct_name)
                    .cloned();

                let mut final_fields = Vec::new();

                if let Some(def) = target_def {
                    for (f_name, mut f_expr, f_ty) in resolved_fields {
                        if let Some((_, expected_ty)) =
                            def.fields.iter().find(|(n, _)| n == &f_name)
                        {
                            if let Type::Struct(names, _) = expected_ty {
                                if let Some(inner_name) = names.first() {
                                    if inner_name.starts_with("__Union_")
                                        && !inner_name.contains("__UnionInner_")
                                    {
                                        if &f_ty != expected_ty {
                                            if let Some(variants) =
                                                self.variant_cache.get(inner_name).cloned()
                                            {
                                                let (wrapped, _) = self.wrap_expr_for_union(
                                                    f_expr,
                                                    f_ty.clone(),
                                                    &variants,
                                                    inner_name.clone(),
                                                );
                                                f_expr = wrapped;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        final_fields.push((f_name, f_expr));
                    }
                } else {
                    for (n, e, _) in resolved_fields {
                        final_fields.push((n, e));
                    }
                }

                (
                    Expr::StructInit(vec![final_struct_name.clone()], final_fields, vec![]),
                    Type::Struct(vec![final_struct_name], vec![]),
                )
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

            Lit::Null => (Expr::Lit(lit), Type::Pointer(Box::new(Type::Void))),
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

            if let Type::Union(variants) = field_ty {
                let union_struct_name = self.get_or_create_union_struct(variants);
                *field_ty = Type::Struct(vec![union_struct_name], vec![]);
            }
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

            Type::Union(variants) => {
                for variant in variants {
                    self.substitute_type(variant, map);
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
