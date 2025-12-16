use crate::{
    hir::FlatProgram,
    lir::{LirFunctionDef, LirProgram, LirType},
};

pub struct Ir;

impl Ir {
    pub fn build(flat_ast: &FlatProgram) -> LirProgram {
        let mut lir = LirProgram::default();
        for f in flat_ast.functions.iter() {
            lir.functions.push(LirFunctionDef {
                body: vec![],
                is_extern: false,
                name: f.name.clone(),
                params: vec![],
                return_type: LirType::Void,
            });
        }
        lir
    }
}
