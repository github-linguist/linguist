def shuffle[T](a: Array[T]) = {
  for (i <- 1 until a.size reverse) {
    val j = util.Random nextInt (i + 1)
    val t = a(i)
    a(i) = a(j)
    a(j) = t
  }
  a
}
