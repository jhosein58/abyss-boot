use std::collections::HashMap;

use crate::{
    hir::FlatProgram,
    lir::{
        LirExpr, LirFunctionDef, LirGlobalVar, LirLiteral, LirProgram, LirStmt, LirStructDef,
        LirType,
    },
    symbols::Context,
};
use abyss_parser::ast::{Expr, FunctionBody, FunctionDef, Lit, StaticDef, Stmt, StructDef, Type};

pub struct Ir {
    ctx: Context,
    local_scope: Vec<HashMap<String, LirType>>,
}

impl Ir {
    pub fn build(flat_ast: &FlatProgram, ctx: Context) -> LirProgram {
        let mut ir_builder = Ir {
            ctx,
            local_scope: vec![std::collections::HashMap::new()],
        };

        let mut lir = LirProgram::default();

        for s in &flat_ast.structs {
            lir.structs.push(ir_builder.transpile_struct(s));
        }

        for s in &flat_ast.statics {
            lir.globals.push(ir_builder.transpile_static(s));
        }

        for func in &flat_ast.functions {
            lir.functions.push(ir_builder.transpile_function(func));
        }

        lir
    }

    fn enter_local_scope(&mut self) {
        self.local_scope.push(std::collections::HashMap::new());
    }

    fn exit_local_scope(&mut self) {
        self.local_scope.pop();
    }

    fn register_local_var(&mut self, name: String, ty: LirType) {
        if let Some(scope) = self.local_scope.last_mut() {
            scope.insert(name, ty);
        }
    }

    fn lookup_type(&self, name: &str) -> Option<LirType> {
        for scope in self.local_scope.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty.clone());
            }
        }
        self.ctx.lookup_var(name).map(|v| v.ty.clone())
    }

    fn resolve_expr_type(&self, expr: &Expr) -> LirType {
        match expr {
            Expr::Ident(path) => {
                let name = path.last().expect("Empty path").clone();
                self.lookup_type(&name).unwrap_or(LirType::Void)
            }
            Expr::Member(lhs, field_name) => {
                let lhs_type = self.resolve_expr_type(lhs);
                if let LirType::Struct(struct_name) = lhs_type {
                    self.ctx
                        .get_struct_field_type(&struct_name, field_name)
                        .unwrap_or(LirType::Void)
                } else if let LirType::Pointer(inner) = lhs_type {
                    if let LirType::Struct(struct_name) = *inner {
                        self.ctx
                            .get_struct_field_type(&struct_name, field_name)
                            .unwrap_or(LirType::Void)
                    } else {
                        LirType::Void
                    }
                } else {
                    LirType::Void
                }
            }
            Expr::StructInit(path, _, _) => LirType::Struct(path.join("__")),
            Expr::MethodCall(lhs, method_name, _, _) => {
                let lhs_type = self.resolve_expr_type(lhs);
                let struct_name = match &lhs_type {
                    LirType::Struct(n) => n,
                    LirType::Pointer(b) => {
                        if let LirType::Struct(n) = &**b {
                            n
                        } else {
                            return LirType::Void;
                        }
                    }
                    _ => return LirType::Void,
                };
                let mangled_func_name = format!("{}__{}", struct_name, method_name);

                if let Some(func_info) = self.ctx.lookup_function(&mangled_func_name) {
                    func_info.ret_ty.clone()
                } else {
                    LirType::Void
                }
            }
            Expr::Lit(Lit::Int(_)) => LirType::I64,
            Expr::Lit(Lit::Float(_)) => LirType::F64,
            Expr::Lit(Lit::Bool(_)) => LirType::Bool,
            Expr::Cast(_, ty) => self.transpile_type(ty),
            _ => LirType::Void,
        }
    }

    fn transpile_type(&self, ty: &Type) -> LirType {
        match ty {
            Type::U8 => LirType::U8,
            Type::U16 => LirType::U16,
            Type::U32 => LirType::U32,
            Type::U64 => LirType::U64,
            Type::Usize => LirType::Usize,
            Type::I8 => LirType::I8,
            Type::I16 => LirType::I16,
            Type::I32 => LirType::I32,
            Type::I64 => LirType::I64,
            Type::Isize => LirType::Isize,
            Type::F32 => LirType::F32,
            Type::F64 => LirType::F64,
            Type::Char => LirType::Char,
            Type::Bool => LirType::Bool,
            Type::Void => LirType::Void,
            Type::Pointer(inner) => LirType::Pointer(Box::new(self.transpile_type(inner))),
            Type::Const(inner) => LirType::Const(Box::new(self.transpile_type(inner))),
            Type::Array(inner, size) => LirType::Array(Box::new(self.transpile_type(inner)), *size),
            Type::Struct(path, _) => LirType::Struct(path.join("__")),
            Type::Function(args, ret, _) => {
                let lir_args = args.iter().map(|t| self.transpile_type(t)).collect();
                let lir_ret = self.transpile_type(ret);
                LirType::FunctionPtr(lir_args, Box::new(lir_ret))
            }
            Type::Generic(_) => LirType::Void,
        }
    }

    fn transpile_struct(&self, def: &StructDef) -> LirStructDef {
        let fields = def
            .fields
            .iter()
            .map(|(name, ty)| (name.clone(), self.transpile_type(ty)))
            .collect();

        LirStructDef {
            name: def.name.clone(),
            fields,
        }
    }

    fn transpile_static(&self, def: &StaticDef) -> LirGlobalVar {
        LirGlobalVar {
            name: def.name.clone(),
            ty: self.transpile_type(&def.ty),
            init_value: Some(self.transpile_expr(&def.value)),
        }
    }

    fn transpile_function(&mut self, func: &FunctionDef) -> LirFunctionDef {
        self.local_scope.clear();
        self.enter_local_scope();

        let params: Vec<(String, LirType)> = func
            .params
            .iter()
            .map(|(n, t)| {
                let lir_ty = self.transpile_type(t);
                self.register_local_var(n.clone(), lir_ty.clone());
                (n.clone(), lir_ty)
            })
            .collect();

        let (body, is_extern) = match &func.body {
            FunctionBody::UserDefined(stmts) => (self.transpile_block(stmts), false),
            FunctionBody::Extern => (vec![], true),
        };

        self.exit_local_scope();

        LirFunctionDef {
            name: func.name.clone(),
            params,
            return_type: self.transpile_type(&func.return_type),
            body,
            is_extern,
            is_variadic: func.is_variadic,
        }
    }

    fn transpile_block(&mut self, stmts: &[Stmt]) -> Vec<LirStmt> {
        self.enter_local_scope();
        let mut result = Vec::new();
        for stmt in stmts {
            result.extend(self.transpile_stmt(stmt));
        }
        self.exit_local_scope();
        result
    }

    fn transpile_stmt(&mut self, stmt: &Stmt) -> Vec<LirStmt> {
        match stmt {
            Stmt::Let(name, ty_opt, expr_opt) => {
                let lir_ty = if let Some(t) = ty_opt {
                    self.transpile_type(t)
                } else {
                    if let Some(expr) = expr_opt {
                        self.resolve_expr_type(expr)
                    } else {
                        LirType::I64
                    }
                };

                self.register_local_var(name.clone(), lir_ty.clone());

                let lir_init = expr_opt.as_ref().map(|e| self.transpile_expr(e));
                vec![LirStmt::Let(name.clone(), lir_ty, lir_init)]
            }

            Stmt::Const(name, ty_opt, expr_opt) => {
                let lir_ty = ty_opt
                    .as_ref()
                    .map(|t| self.transpile_type(t))
                    .unwrap_or(LirType::I64);
                self.register_local_var(name.clone(), lir_ty.clone());
                let lir_init = expr_opt.as_ref().map(|e| self.transpile_expr(e));
                vec![LirStmt::Let(name.clone(), lir_ty, lir_init)]
            }

            Stmt::Block(inner_stmts) => {
                vec![LirStmt::Block(self.transpile_block(inner_stmts))]
            }

            Stmt::If(cond, then_box, else_box) => {
                let lir_cond = self.transpile_expr(cond);
                let then_branch = self.transpile_stmt(then_box);
                let else_branch = if let Some(else_stmt) = else_box {
                    self.transpile_stmt(else_stmt)
                } else {
                    vec![]
                };
                vec![LirStmt::If {
                    cond: lir_cond,
                    then_branch,
                    else_branch,
                }]
            }

            Stmt::While(cond, body) => {
                vec![LirStmt::While {
                    cond: self.transpile_expr(cond),
                    body: self.transpile_stmt(body),
                }]
            }

            Stmt::Assign(lhs, rhs) => vec![LirStmt::Assign(
                self.transpile_expr(lhs),
                self.transpile_expr(rhs),
            )],
            Stmt::Expr(e) => vec![LirStmt::ExprStmt(self.transpile_expr(e))],
            Stmt::Ret(e) => vec![LirStmt::Return(Some(self.transpile_expr(e)))],
            Stmt::Break => vec![LirStmt::Break],
            Stmt::Continue => vec![LirStmt::Continue],
            _ => vec![],
        }
    }

    fn transpile_expr(&self, expr: &Expr) -> LirExpr {
        match expr {
            Expr::Lit(Lit::Array(elements)) => {
                let lir_elems = elements.iter().map(|e| self.transpile_expr(e)).collect();

                LirExpr::ArrayInit(lir_elems)
            }

            Expr::Lit(Lit::Str(v)) => {
                let mut lir_elems = Vec::new();

                let content = if v.starts_with('"') && v.ends_with('"') && v.len() >= 2 {
                    &v[1..v.len() - 1]
                } else {
                    v.as_str()
                };

                let mut chars = content.chars().peekable();

                while let Some(c) = chars.next() {
                    if c == '\\' {
                        if let Some(next_char) = chars.next() {
                            match next_char {
                                'n' => lir_elems.push(LirExpr::Lit(LirLiteral::Byte(10))),
                                'r' => lir_elems.push(LirExpr::Lit(LirLiteral::Byte(13))),
                                't' => lir_elems.push(LirExpr::Lit(LirLiteral::Byte(9))),
                                '0' => lir_elems.push(LirExpr::Lit(LirLiteral::Byte(0))),
                                '\\' => lir_elems.push(LirExpr::Lit(LirLiteral::Byte(92))),
                                '"' => lir_elems.push(LirExpr::Lit(LirLiteral::Byte(34))),
                                other => {
                                    lir_elems.push(LirExpr::Lit(LirLiteral::Byte(other as u8)));
                                }
                            }
                        } else {
                            lir_elems.push(LirExpr::Lit(LirLiteral::Byte(92)));
                        }
                    } else {
                        lir_elems.push(LirExpr::Lit(LirLiteral::Byte(c as u8)));
                    }
                }

                lir_elems.push(LirExpr::Lit(LirLiteral::Byte(0)));

                LirExpr::ArrayInit(lir_elems)
            }

            Expr::Lit(l) => LirExpr::Lit(self.transpile_lit(l)),
            Expr::Ident(path) => LirExpr::Ident(path.join("__")),
            Expr::Binary(l, op, r) => LirExpr::Binary(
                Box::new(self.transpile_expr(l)),
                *op,
                Box::new(self.transpile_expr(r)),
            ),
            Expr::Unary(op, inner) => LirExpr::Unary(*op, Box::new(self.transpile_expr(inner))),
            Expr::Call(callee, args, _) => {
                let lir_args = args.iter().map(|a| self.transpile_expr(a)).collect();
                match &**callee {
                    Expr::Ident(path) => LirExpr::Call {
                        func_name: path.join("__"),
                        args: lir_args,
                    },
                    _ => LirExpr::CallPtr(Box::new(self.transpile_expr(callee)), lir_args),
                }
            }

            Expr::Member(obj, field) => {
                let lhs_lir = self.transpile_expr(obj);
                let lhs_type = self.resolve_expr_type(obj);

                if let LirType::Pointer(_) = lhs_type {
                    LirExpr::MemberAccessPtr(Box::new(lhs_lir), field.clone())
                } else {
                    LirExpr::MemberAccess(Box::new(lhs_lir), field.clone())
                }
            }

            Expr::MethodCall(obj, method_name, args, _) => {
                let obj_type = self.resolve_expr_type(obj);

                let struct_name = match &obj_type {
                    LirType::Struct(name) => name.clone(),
                    LirType::Pointer(inner) => {
                        if let LirType::Struct(name) = &**inner {
                            name.clone()
                        } else {
                            panic!("Method call on non-struct pointer")
                        }
                    }
                    _ => panic!("Method call on non-struct type: {:?}", obj_type),
                };

                let mangled_func_name = format!("{}__{}", struct_name, method_name);

                let mut lir_args = Vec::new();

                let obj_lir = self.transpile_expr(obj);
                if let LirType::Struct(_) = obj_type {
                    lir_args.push(LirExpr::AddrOf(Box::new(obj_lir)));
                } else {
                    lir_args.push(obj_lir);
                }

                for arg in args {
                    lir_args.push(self.transpile_expr(arg));
                }

                LirExpr::Call {
                    func_name: mangled_func_name,
                    args: lir_args,
                }
            }

            Expr::StructInit(path, fields, _) => {
                let lir_fields = fields
                    .iter()
                    .map(|(n, e)| (n.clone(), self.transpile_expr(e)))
                    .collect();
                LirExpr::StructInit {
                    struct_name: path.join("__"),
                    fields: lir_fields,
                }
            }
            Expr::Index(arr, idx) => LirExpr::Index(
                Box::new(self.transpile_expr(arr)),
                Box::new(self.transpile_expr(idx)),
            ),
            Expr::Deref(i) => LirExpr::Deref(Box::new(self.transpile_expr(i))),
            Expr::AddrOf(i) => LirExpr::AddrOf(Box::new(self.transpile_expr(i))),
            Expr::Cast(i, t) => {
                LirExpr::Cast(Box::new(self.transpile_expr(i)), self.transpile_type(t))
            }
            Expr::SizeOf(t) => LirExpr::SizeOf(self.transpile_type(t)),
            _ => LirExpr::Lit(LirLiteral::Null),
        }
    }

    fn transpile_lit(&self, lit: &Lit) -> LirLiteral {
        match lit {
            Lit::Int(v) => LirLiteral::Int(*v),
            Lit::Float(v) => LirLiteral::Float(*v),
            Lit::Bool(v) => LirLiteral::Bool(*v),
            Lit::Str(v) => {
                let mut bytes: Vec<LirLiteral> =
                    v.bytes().map(|b| LirLiteral::Int(b as i64)).collect();

                bytes.push(LirLiteral::Int(0));

                LirLiteral::Array(bytes)
            }

            Lit::Null => LirLiteral::Null,
            Lit::Array(_) => LirLiteral::Null,
        }
    }
}
