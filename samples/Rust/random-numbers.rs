use std::rand;
use std::rand::distributions::{Normal, IndependentSample};

fn main() {
    let mut rands = [0.0, ..1000];
    let normal = Normal::new(1.0, 0.5);

    for i in range(0, 1000) {
        let v = normal.ind_sample(&mut rand::task_rng());
        rands[i] = v;
    }
}
