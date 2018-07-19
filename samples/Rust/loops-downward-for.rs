// rust 0.9-pre

fn main() {
    for i in std::iter::range_inclusive(0, 10).invert() {
        println!("{}", i);
    }
}
