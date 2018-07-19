use std::rand;
use std::rand::Rng;

fn main() {
  let mut rng = rand::task_rng();
  loop {
    let num = rng.gen_range(0, 20);
    println!("{:d}", num);
    if num == 10 {
      break;
    }
    println!("{:d}", rng.gen_range(0, 20));
  }
}
