use std::iter::range_step_inclusive;

fn main() {
  for i in range_step_inclusive(2, 8, 2) {
    print!("{:d}, ", i);
  }
  println("who do we appreciate?!");
}
