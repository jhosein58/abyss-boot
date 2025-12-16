use crate::lir::LirType;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub params: Vec<LirType>,
    pub ret_ty: LirType,
}

#[derive(Debug, Clone)]
pub struct VarInfo {
    pub ty: LirType,
}

pub struct Context {
    pub functions: HashMap<String, FunctionInfo>,
    pub vars: Vec<HashMap<String, VarInfo>>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            vars: vec![HashMap::new()],
        }
    }

    pub fn register_function(&mut self, name: String, params: Vec<LirType>, ret_ty: LirType) {
        self.functions.insert(name, FunctionInfo { params, ret_ty });
    }

    pub fn lookup_function(&self, name: &str) -> Option<&FunctionInfo> {
        self.functions.get(name)
    }

    pub fn enter_scope(&mut self) {
        self.vars.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.vars.pop();
    }

    pub fn declare_var(&mut self, name: String, ty: LirType) {
        if let Some(scope) = self.vars.last_mut() {
            scope.insert(name, VarInfo { ty });
        }
    }

    pub fn lookup_var(&self, name: &str) -> Option<&VarInfo> {
        for scope in self.vars.iter().rev() {
            if let Some(info) = scope.get(name) {
                return Some(info);
            }
        }
        None
    }

    pub fn clear_vars(&mut self) {
        self.vars = vec![HashMap::new()];
    }
}
