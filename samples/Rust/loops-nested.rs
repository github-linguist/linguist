// rust 0.9-pre

use std::rand::Rng;

fn main() {
    let mut matrix = [[0u8, .. 10], .. 10];
    let mut rng = std::rand::os::OSRng::new();

    for row in matrix.mut_iter() {
        for item in row.mut_iter() {
            *item = rng.gen_range(0u8, 21);
        }
    }

    'outer:
    for row in matrix.iter() {
        for &item in row.iter() {
            print!("{:2} ", item);
            if item == 20 { break 'outer; }
        }
        println!("");
    }
}
