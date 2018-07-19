for value in 0..33 {
  for base in [2, 8, 10, 12, 16, 36] {
    def s := value.toString(base)
    print(" " * (8 - s.size()), s)
  }
  println()
}
