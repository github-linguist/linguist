def biased( n:Int ) = scala.util.Random.nextFloat < 1.0 / n

def unbiased( n:Int ) = { def loop : Boolean = { val a = biased(n); if( a != biased(n) ) a else loop }; loop }

for( i <- (3 until 7) ) println {

  val m = 50000
  var c1,c2 = 0

  (0 until m) foreach { j => if( biased(i) ) c1 += 1; if( unbiased(i) ) c2 += 1 }

  "%d: %2.2f%%  %2.2f%%".format(i, 100.0*c1/m, 100.0*c2/m)
}
