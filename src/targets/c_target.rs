use crate::targets::CompilationTarget;

pub struct CTarget {
    pub buffer: String,
    indent: usize,
}

impl CTarget {
    pub fn new() -> Self {
        Self {
            buffer: String::from(include_str!("../targets/runtime/c_runtime.c")),
            indent: 0,
        }
    }
    fn write(&mut self, s: &str) {
        let spaces = "    ".repeat(self.indent);
        self.buffer.push_str(&format!("{}{}\n", spaces, s));
    }
}

impl CompilationTarget for CTarget {
    type Output = String;

    fn func_def(&mut self, name: &str, args: &[&str]) {
        let target_name = if name == "main" {
            "abyss_user_main"
        } else {
            name
        };

        let args_str: Vec<String> = args.iter().map(|a| format!("AbyssValue {}", a)).collect();
        self.write(&format!(
            "AbyssValue {}({}) {{",
            target_name,
            args_str.join(", ")
        ));
        self.indent += 1;
    }

    fn build_entry_point(&mut self, user_main_name: &str) {
        let target_func_call = if user_main_name == "main" {
            "abyss_user_main"
        } else {
            user_main_name
        };

        self.write("\n// --- Entry Point ---");
        self.write("int main(int argc, char** argv) {");
        self.indent += 1;

        self.write(&format!("AbyssValue result = {}();", target_func_call));

        self.write("if (result.type != TYPE_NULL) {");
        self.write("    print(result);");
        self.write("}");

        self.write("return 0;");

        self.indent -= 1;
        self.write("}");
    }

    fn func_end(&mut self) {
        self.write("return create_null();"); // Safety return
        self.indent -= 1;
        self.write("}\n");
    }

    fn var_declare(&mut self, name: &str, val: Self::Output) {
        self.write(&format!("AbyssValue {} = {};", name, val));
    }

    fn var_assign(&mut self, name: &str, val: Self::Output) {
        self.write(&format!("{} = {};", name, val));
    }

    fn ret(&mut self, val: Self::Output) {
        self.write(&format!("return {};", val));
    }

    fn expr_stmt(&mut self, val: Self::Output) {
        self.write(&format!("{};", val));
    }

    fn add(&mut self, l: Self::Output, r: Self::Output) -> Self::Output {
        format!("abyss_add({}, {})", l, r)
    }

    fn sub(&mut self, l: Self::Output, r: Self::Output) -> Self::Output {
        format!("abyss_sub({}, {})", l, r)
    }
    fn mul(&mut self, l: Self::Output, r: Self::Output) -> Self::Output {
        format!("abyss_mul({}, {})", l, r)
    }
    fn div(&mut self, l: Self::Output, r: Self::Output) -> Self::Output {
        format!("abyss_div({}, {})", l, r)
    }

    fn number(&mut self, val: f64) -> Self::Output {
        format!("create_number({:.2})", val)
    }

    fn string(&mut self, val: &str) -> Self::Output {
        format!("create_string(\"{}\")", val)
    }

    fn variable(&mut self, name: &str) -> Self::Output {
        name.to_string()
    }

    fn call(&mut self, func: &str, args: Vec<Self::Output>) -> Self::Output {
        format!("{}({})", func, args.join(", "))
    }
}
