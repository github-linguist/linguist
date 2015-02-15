// rustc 0.9 (7613b15 2014-01-08 18:04:43 -0800)

#[feature(non_ascii_idents)];

fn main() {
    let mut Δ:int = 1;
    Δ += 1;
    println!("{}", Δ);
}
