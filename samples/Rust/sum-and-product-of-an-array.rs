use std::iter::{AdditiveIterator, MultiplicativeIterator};

fn main() {
  let arr = [1, 2, 3, 4, 5, 6, 7, 8, 9];

  // using fold
  let sum = arr.iter().fold(0, |a, &b| a + b);
  let product = arr.iter().fold(1, |a, &b| a * b);
  println!("the sum is {:d} and the product is {:d}", sum, product);

  // or using sum and product from AdditiveIterator
  // and MultiplicativeIterator
  let sum2 = arr.iter().map(|&a| a).sum();
  let product2 = arr.iter().map(|&a| a).product();
  println!("the sum is {:d} and the product is {:d}", sum2, product2);
}
