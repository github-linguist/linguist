extern crate collections;

use std::str;
use collections::HashMap;
use std::io::File;
use std::io::BufferedReader;
use std::cmp;

fn sort_string(string: &str) -> ~str {
	let mut chars = string.chars().to_owned_vec();
	chars.sort();
	str::from_chars(chars)
}

fn main () {
	let path = Path::new("unixdict.txt");
	let mut file = BufferedReader::new(File::open(&path));

	let mut map: HashMap<~str, ~[~str]> = HashMap::new();

	for line in file.lines() {
		let s = line.trim().to_owned();
		map.mangle(sort_string(s.clone()), s,
				   |_k, v| ~[v],
				   |_k, v, string| v.push(string)
				);
	}

	let max_length = map.iter().fold(0, |s, (_k, v)| cmp::max(s, v.len()));

	for (_k, v) in map.iter() {
		if v.len() == max_length {
			for s in v.iter() {
				print!("{} ", *s)
			}
			println!("")
		}
	}
}
