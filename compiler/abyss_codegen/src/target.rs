use abyss_analyzer::lir::LirType;

pub trait Target {
    // --- 1. Global Definitions ---
    fn start_program(&mut self);
    fn end_program(&mut self);

    fn declare_extern_function(
        &mut self,
        name: &str,
        params: &[(String, LirType)],
        return_type: &LirType,
    );

    fn declare_function_proto(
        &mut self,
        name: &str,
        params: &[(String, LirType)],
        return_type: &LirType,
    );

    // --- 2. Function Scope ---
    fn begin_function(&mut self, name: &str, params: &[(String, LirType)], return_type: &LirType);
    fn end_function(&mut self);
}
