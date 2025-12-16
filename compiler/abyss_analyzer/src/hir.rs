use abyss_parser::ast::{EnumDef, FunctionDef, StaticDef, StructDef};

#[derive(Debug, Clone)]
pub struct FlatProgram {
    pub functions: Vec<FunctionDef>,
    pub structs: Vec<StructDef>,
    pub enums: Vec<EnumDef>,
    pub statics: Vec<StaticDef>,
}

impl FlatProgram {
    pub fn new() -> Self {
        Self {
            functions: vec![],
            structs: vec![],
            enums: vec![],
            statics: vec![],
        }
    }
}
