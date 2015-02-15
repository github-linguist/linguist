/** Find factors of a Mersenne number
 *
 *  The implementation finds factors for M929 and further.
 *
 *  @example  M59 = 2^059 - 1 =             576460752303423487  (   2 msec)
 *  @example        = 179951 × 3203431780337.
 */
object FactorMersenne extends App {

  val two: BigInt = 2

  def sieve(nums: Stream[Int]): Stream[Int] =
    Stream.cons(nums.head, sieve((nums.tail) filter (_ % nums.head != 0)))
  // An infinite stream of primes, lazy evaluation and memo-ized
  val oddPrimes = sieve(Stream.from(3, 2))
  def primes = sieve(2 #:: oddPrimes)

  def mersenne(p: Int) = (two pow p) - 1

  def factorMersenne(p: Int): Option[Long] = {
    val limit = (mersenne(p) - 1 min Int.MaxValue).toLong

    def factorTest(p: Long, q: Long): Boolean = {
      (List(1, 7) contains (q % 8)) && two.modPow(p, q) == 1 && BigInt(q).isProbablePrime(7)
    }

    // Build a stream of factors from (2*p+1) step-by (2*p)
    def s(a: Long): Stream[Long] = a #:: s(a + (2 * p)) // Build stream of possible factors

    // Limit and Filter Stream and then take the head element
    val e = s(2 * p + 1).takeWhile(_ < limit).filter(factorTest(p, _))
    e.headOption
  }

  // Test
  (primes takeWhile (_ <= 97)) ++ List(929, 937) foreach { p =>
    { // Needs some intermediate results for nice formatting
      val nMersenne = mersenne(p); val lit = f"${nMersenne}%30d"
      val preAmble = f"${s"M${p}"}%4s = 2^$p%03d - 1 = ${lit}%s"

      val datum = System.nanoTime
      val result = factorMersenne(p)
      val mSec = ((System.nanoTime - datum) / 1.e+6).round

      def decStr = { if (lit.length > 30) f"(M has ${lit.length}%3d dec)" else "" }
      def sPrime = { if (result.isEmpty) " is a Mersenne prime number." else " " * 28 }

      println(f"$preAmble${sPrime} ${f"($mSec%,1d"}%13s msec)")
      if (!result.isEmpty)
        println(f"${decStr}%-17s = ${result.get} × ${nMersenne / result.get}")
    }
  }
}
