// rust 0.9-pre

fn main() {
    let a1 = ["a", "b", "c"];
    let a2 = ["A", "B", "C"];
    let a3 = [1, 2, 3];

    for ((&x, &y), &z) in a1.iter().zip(a2.iter()).zip(a3.iter()) {
        println!("{}{}{}", x, y, z);
    }
}
