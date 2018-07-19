def F(n:Int):Int =
  if (n == 0) 1 else n - M(F(n-1))
def M(n:Int):Int =
  if (n == 0) 0 else n - F(M(n-1))

println((0 until 20).map(F).mkString(", "))
println((0 until 20).map(M).mkString(", "))
