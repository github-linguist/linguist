object IntCompare {
  def main(args: Array[String]): Unit = {
    val a=Console.readInt
    val b=Console.readInt
    if (a < b)
      printf("%d is less than %d\n", a, b)
    if (a == b)
      printf("%d is equal to %d\n", a, b)
    if (a > b)
      printf("%d is greater than %d\n", a, b)
  }
}
