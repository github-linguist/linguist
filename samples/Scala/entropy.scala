import scala.math._

def entropy( v:String ) = { v
  .groupBy (a => a)
  .values
  .map( i => i.length.toDouble / v.length )
  .map( p => -p * log10(p) / log10(2))
  .sum
}

// Confirm that "1223334444" has an entropy of about 1.84644
assert( math.round( entropy("1223334444") * 100000 ) * 0.00001 == 1.84644 )
