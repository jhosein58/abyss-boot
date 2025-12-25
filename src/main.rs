use abyss::{Abyss, CTarget};

fn main() {
    let path = "main.a";

    let code = std::fs::read_to_string(path).expect("Failed to read main file");
    let mut abyss = Abyss::new(&code, path, CTarget::new());

    abyss.run();
}
