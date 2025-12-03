pub trait CompilationTarget {
    type Output;

    fn build_entry_point(&mut self, user_main_name: &str);

    fn func_def(&mut self, name: &str, args: &[&str]);
    fn func_end(&mut self);

    fn var_declare(&mut self, name: &str, val: Self::Output);
    fn var_assign(&mut self, name: &str, val: Self::Output);

    fn add(&mut self, l: Self::Output, r: Self::Output) -> Self::Output;
    fn sub(&mut self, l: Self::Output, r: Self::Output) -> Self::Output;
    fn mul(&mut self, l: Self::Output, r: Self::Output) -> Self::Output;
    fn div(&mut self, l: Self::Output, r: Self::Output) -> Self::Output;

    fn number(&mut self, val: f64) -> Self::Output;
    fn string(&mut self, val: &str) -> Self::Output;
    fn variable(&mut self, name: &str) -> Self::Output;

    fn call(&mut self, func: &str, args: Vec<Self::Output>) -> Self::Output;
    fn ret(&mut self, val: Self::Output);

    fn expr_stmt(&mut self, val: Self::Output);
}

pub mod c_target;
