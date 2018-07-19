// rust 0.9-pre

fn main() {
    let s = "-1";
    let s = (from_str::<int>(s).unwrap() + 1).to_str();
    assert_eq!(s, ~"0");
    println(s);
}
