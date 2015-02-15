def arithmeticMean(n: Seq[Int]) = n.sum / n.size.toDouble
def geometricMean(n: Seq[Int])  = math.pow(n.foldLeft(1.0)(_*_), 1.0 / n.size.toDouble)
def harmonicMean(n: Seq[Int])   = n.size / n.map(1.0 / _).sum

var nums = 1 to 10

var a = arithmeticMean(nums)
var g = geometricMean(nums)
var h = harmonicMean(nums)

println("Arithmetic mean " + a)
println("Geometric mean  " + g)
println("Harmonic mean   " + h)

assert(a >= g && g >= h)
