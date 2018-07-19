scala> val x = 2.0
x: Double = 2.0

scala> val xi = 0.5
xi: Double = 0.5

scala> val y = 4.0
y: Double = 4.0

scala> val yi = 0.25
yi: Double = 0.25

scala> val z = x + y
z: Double = 6.0

scala> val zi = 1.0 / ( x + y )
zi: Double = 0.16666666666666666

scala> val numbers = List(x, y, z)
numbers: List[Double] = List(2.0, 4.0, 6.0)

scala> val inverses = List(xi, yi, zi)
inverses: List[Double] = List(0.5, 0.25, 0.16666666666666666)

scala> def multiplier = (n1: Double, n2: Double) => (m: Double) => n1 * n2 * m
multiplier: (Double, Double) => (Double) => Double

scala> def comp = numbers zip inverses map multiplier.tupled
comp: List[(Double) => Double]

scala> comp.foreach(f=>println(f(0.5)))
0.5
0.5
0.5
