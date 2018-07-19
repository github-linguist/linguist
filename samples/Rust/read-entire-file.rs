// -*- rust v0.9 -*-
use std::str;
use std::io::File;

fn main() {
	let path = Path::new("hello.txt");
	let mut file = File::open(&path);
	
	let contents = str::from_utf8_owned(file.read_to_end());
	println(contents);
}
