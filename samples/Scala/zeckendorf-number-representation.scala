def zNum( n:BigInt ) : String = {

  if( n == 0 ) return "0"	// Short-circuit this and return zero if we were given zero


  val v = n.abs

  val fibs : Stream[BigInt] = { def series(i:BigInt,j:BigInt):Stream[BigInt] = i #:: series(j, i+j); series(1,0).tail.tail.tail }


  def z( v:BigInt ) : List[BigInt] = if(v == 0) List() else {val m = fibs(fibs.indexWhere(_>v) - 1); m :: z(v-m)}

  val zv = z(v)

  // Walk the list of fibonacci numbers from the number that matches the most significant down to 1,
  // if the zeckendorf matchs then yield '1' otherwise '0'
  val s = (for( i <- (fibs.indexWhere(_==zv(0)) to 0 by -1) ) yield {

    if( zv.contains(fibs(i))) "1" else "0"
	
  }).mkString

  if( n < 0 ) "-" + s		// Using a negative-sign instead of twos-complement
  else s
}


// A little test...
(0 to 20) foreach( i => print( zNum(i) + "\n" ) )
