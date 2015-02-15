// rust 0.9-pre

use std::uint;

fn main() {
    for i in range(uint::min_value, uint::max_value) {
        println!("{:o}", i);
    }
}
