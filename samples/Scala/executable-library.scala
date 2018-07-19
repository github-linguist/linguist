object HailstoneSequence extends App { // Show it all, default number is 27.
  def hailstone(n: Int): Stream[Int] =
       n #:: (if (n == 1) Stream.empty else hailstone(if (n % 2 == 0) n / 2 else n * 3 + 1))

  Hailstone.details(args.headOption.map(_.toInt).getOrElse(27))
  HailTest.main(Array())
}

object Hailstone extends App { // Compute a given or default number to Hailstone sequence
  def details(nr: Int) = {
    val collatz = HailstoneSequence.hailstone(nr)

    println(s"Use the routine to show that the hailstone sequence for the number: $nr.")
    println(collatz.toList)
    println(s"It has ${collatz.length} elements.")
  }
  details(args.headOption.map(_.toInt).getOrElse(27))
}

object HailTest extends App { // Compute only the < 100000 test
  println(
    "Compute the number < 100,000, which has the longest hailstone sequence with that sequence's length.")
  val (n, len) = (1 until 100000).map(n => (n, HailstoneSequence.hailstone(n).length)).maxBy(_._2)
  println(s"Longest hailstone sequence length= $len occurring with number $n.")
}
