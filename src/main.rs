use abyss::{Abyss, CTarget};

fn main() {
    let mut abyss = Abyss::new(include_str!("../main.a"), "main.a", CTarget::new());
    abyss.run();
    //println!("{}", abyss.emit())
}
