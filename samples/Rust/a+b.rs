// -*- rust v0.9 -*-
use std::os;

fn main() {
    	let args : ~[~str] = os::args();
	let mut values = 0;

	for i in args.iter(){
		match from_str::<int>(i.to_str()) {
			Some(valid_int) => values += valid_int,
			None => ()
		}
	}
	println(values.to_str());
}
