object LLT extends App {
  import Stream._

  def primeSieve(s: Stream[Int]): Stream[Int] =
    s.head #:: primeSieve(s.tail filter { _ % s.head != 0 })
  val primes = primeSieve(from(2))

  def mersenne(p: Int): BigInt = (BigInt(2) pow p) - 1

  def s(mp: BigInt, p: Int): BigInt = { if (p == 1) 4 else ((s(mp, p - 1) pow 2) - 2) % mp }

  val upbPrime = 9999
  println(s"Finding Mersenne primes in M[2..$upbPrime]")
  ((primes takeWhile (_ <= upbPrime)).par map { p => (p, mersenne(p)) }
    map { p => if (p._1 == 2) (p, 0) else (p, s(p._2, p._1 - 1)) } filter { _._2 == 0 })
    .foreach { p =>
      println(s"prime M${(p._1)._1}: " +
        { if ((p._1)._1 < 200) (p._1)._2 else s"(${(p._1)._2.toString.size} digits)" })
    }
  println("That's All Folks!")
}
