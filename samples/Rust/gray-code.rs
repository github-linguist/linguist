fn gray_encode(integer: uint) -> uint {
	(integer >> 1) ^ integer
}

fn gray_decode(integer: uint) -> uint {
	match integer {
		0 => 0,
		_ => integer ^ gray_decode(integer >> 1)
	}
}


fn main() {
	for i in range(0u,32u) {
		println!("{:2} {:0>5t} {:0>5t} {:2}", i, i, gray_encode(i),
			gray_decode(i));
	}

}
