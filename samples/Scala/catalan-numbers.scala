object Catalan {
  def factorial(n: BigInt) = BigInt(1).to(n).foldLeft(BigInt(1))(_ * _)
  def catalan(n: BigInt) = factorial(2 * n) / (factorial(n + 1) * factorial(n))

  def main(args: Array[String]) {
    for (n <- 1 to 15) {
      println("catalan(" + n + ") = " + catalan(n))
    }
  }
}
