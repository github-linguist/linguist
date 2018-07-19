fn move(n: int, from: int, to: int, via: int) {
  if (n > 0) {
    move(n - 1, from, via, to);
    println!("Move disk from pole {:d} to pole {:d}", from, to);
    move(n - 1, via, to, from);
  }
}

fn main() {
  move(4, 1,2,3);
}
