use std::iter;
use std::rand;
use std::rand::Rng;
use std::vec;

fn knuth_shuffle<T>(v: &mut [T]) {
    let mut rng = rand::rng();
    let l = v.len();

    for n in iter::range(0, l) {
        let i = rng.gen_range(0, l - n);
        v.swap(i, l - n - 1);
    }
}

fn main() {
    let mut v = vec::from_fn(10, |i| i);

    println!("before: {:?}", v);
    knuth_shuffle(v);
    println!("after:  {:?}", v);
}
