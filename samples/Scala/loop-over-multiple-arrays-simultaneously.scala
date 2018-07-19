("abc", "ABC", "123").zipped foreach { (x, y, z) =>
  println(x.toString + y + z)
}
