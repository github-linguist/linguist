use std::iter::range_inclusive;

fn main() {
  for i in range_inclusive(1, 10) {
    print!("{:d}", i);
    if i % 5 == 0 {
      print("\n");
      continue;
    }
    print(", ");
  }
}
