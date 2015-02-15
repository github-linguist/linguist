object MonteCarlo {
  private val random = new scala.util.Random

  /** Returns a random number between -1 and 1 */
  def nextThrow: Double = (random.nextDouble * 2.0) - 1.0

  /** Returns true if the argument point would be 'inside' the unit circle with
    * center at the origin, and bounded by a square with side lengths of 2
    * units. */
  def insideCircle(pt: (Double, Double)): Boolean = pt match {
    case (x, y) => (x * x) + (y * y) <= 1.0
  }

  /** Runs the simulation the specified number of times. Uses the result to
    * estimate a value of pi */
  def simulate(times: Int): Double = {
    val inside = Iterator.tabulate (times) (_ => (nextThrow, nextThrow)) count insideCircle
    inside.toDouble / times.toDouble * 4.0
  }

  def main(args: Array[String]): Unit = {
    val sims = Seq(10000, 100000, 1000000, 10000000, 100000000)
    sims.foreach { n =>
      println(n+" simulations; pi estimation: "+ simulate(n))
    }
  }
}
