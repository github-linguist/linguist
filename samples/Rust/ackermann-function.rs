 // works for Rust 0.9
fn main() {
	let a: int = ack(3, 4); // 125
	println!("{}", a.to_str());
}

fn ack(m: int, n: int) -> int {
	if m == 0 {
		n + 1
	} else if n == 0 {
		ack(m - 1, 1)
	} else {
		ack(m - 1, ack(m, n - 1))
	}
}
