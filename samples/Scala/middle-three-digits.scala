/**
 * Optionally return the middle three digits of an integer.
 *
 * @example List(123,12345,-789,1234,12) flatMap (middleThree(_)), returns: List(123, 234, 789)
 */
def middleThree( s:Int ) : Option[Int] = s.abs.toString match {
  case v if v.length % 2 == 0   => None   // Middle three is undefined for even lengths
  case v if v.length < 3        => None
  case v                        => 			
    val i = (v.length / 2) - 1
    Some( v.substring(i,i+3).toInt )
}


// A little test...
val intVals = List(123,12345,1234567,987654321,10001,-10001,-123,-100,100,-12345,1,2,-1,-10,2002,-2002,0)

intVals map (middleThree(_)) map {
  case None => "No middle three"
  case Some(v) => "%03d".format(v)  // Format the value, force leading zeroes
} mkString("\n")
