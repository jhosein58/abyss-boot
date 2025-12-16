use abyss_analyzer::lir::{LirFunctionDef, LirProgram, LirType};

use crate::target::Target;

pub struct Director<'a, T: Target> {
    target: &'a mut T,
}

impl<'a, T: Target> Director<'a, T> {
    pub fn new(target: &'a mut T) -> Self {
        Self { target }
    }

    pub fn process_program(&mut self, program: &LirProgram) {
        self.target.start_program();

        for func in &program.functions {
            let ret_ty = &func.return_type;
            let params = self.get_func_params(func);

            if func.is_extern {
                self.target
                    .declare_extern_function(&func.name, &params, ret_ty);
            } else {
                self.target
                    .declare_function_proto(&func.name, &params, ret_ty);
            }
        }

        for func in &program.functions {
            if !func.is_extern {
                self.compile_function(func);
            }
        }

        self.target.end_program();
    }

    pub fn get_func_params(&self, func: &LirFunctionDef) -> Vec<(String, LirType)> {
        let mut params = Vec::new();
        for (p_name, p_type) in &func.params {
            params.push((p_name.clone(), p_type.clone()));
        }
        params
    }

    fn compile_function(&mut self, func: &LirFunctionDef) {
        let ret_ty = func.return_type.clone();
        let params = self.get_func_params(func);

        self.target.begin_function(&func.name, &params, &ret_ty);

        self.target.end_function();
    }
}
