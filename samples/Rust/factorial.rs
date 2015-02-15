fn factorial_recursive (n: uint) -> uint {
	match n {
		0 => 1,
		_ => n * factorial_recursive(n-1)
	}
}

fn factorial_iterative(n: uint) -> uint {
	range(1u, n+1).fold(1, |p, t| p * t)
}

fn main () {
	for i in range(1u, 10u) {
		println!("{}", factorial_recursive(i))
	}
	for i in range(1u, 10u) {
		println!("{}", factorial_iterative(i))
	}
}
