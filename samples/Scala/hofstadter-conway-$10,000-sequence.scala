object HofstadterConway {
  def pow2(n: Int): Int = (Iterator.fill(n)(2)).product

  def makeHCSequence(max: Int): Seq[Int] =
    (0 to max - 1).foldLeft (Vector[Int]()) { (v, idx) =>
      if (idx <= 1) v :+ 1 else v :+ (v(v(idx - 1) - 1) + v(idx - v(idx - 1)))
    }

  val max = pow2(20)

  val maxSeq = makeHCSequence(max)

  def hcRatio(n: Int, seq: Seq[Int]): Double = seq(n - 1).toDouble / n

  def maximumHCRatioBetween(a: Int, b: Int): (Int, Double) =
    Iterator.range(a, b + 1) map (n => (n, hcRatio(n, maxSeq))) maxBy (_._2)

  lazy val mallowsNumber: Int =
    ((max to 1 by -1) takeWhile (hcRatio(_, maxSeq) < 0.55) last) - 1

  def main(args: Array[String]): Unit = {
    for (n <- 1 to 19) {
      val (value, ratio) = maximumHCRatioBetween(pow2(n), pow2(n+1))
      val message = "Maximum of a(n)/n between 2^%s and 2^%s was %s at %s"
      println(message.format(n, n+1, ratio, value))
    }
    println("Mallow's number = %s".format(mallowsNumber))
  }
}
