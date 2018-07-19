def digitalRoot(x:BigInt, base:Int=10):(Int,Int) = {
  def sumDigits(x:BigInt):Int=x.toString(base) map (_.asDigit) sum
  def loop(s:Int, c:Int):(Int,Int)=if (s < 10) (s, c) else loop(sumDigits(s), c+1)
  loop(sumDigits(x), 1)
}

Seq[BigInt](627615, 39390, 588225, BigInt("393900588225")) foreach {x =>
  var (s, c)=digitalRoot(x)
  println("%d has additive persistance %d and digital root of %d".format(x,c,s))
}
var (s, c)=digitalRoot(0x7e0, 16)
println("%x has additive persistance %d and digital root of %d".format(0x7e0,c,s))
