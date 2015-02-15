def primeFactors( n:Int ) = {

  def primeStream(s: Stream[Int]): Stream[Int] = {
    s.head #:: primeStream(s.tail filter { _ % s.head != 0 })
  }

  val primes = primeStream(Stream.from(2))

  def factors( n:Int ) : List[Int] = primes.takeWhile( _ <= n ).find( n % _ == 0 ) match {
    case None => Nil
    case Some(p) => p :: factors( n/p )
  }

  if( n == 1 ) List(1) else factors(n)
}

// A little test...
{
  val nums = (1 to 12).toList :+ 2144 :+ 6358
  nums.foreach( n => println( "%6d : %s".format( n, primeFactors(n).mkString(" * ") ) ) )
}
