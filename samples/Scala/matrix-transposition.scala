scala> Array.tabulate(4)(i => Array.tabulate(4)(j => i*4 + j))
res12: Array[Array[Int]] = Array(Array(0, 1, 2, 3), Array(4, 5, 6, 7), Array(8, 9, 10, 11), Array(12, 13, 14, 15))

scala> res12.transpose
res13: Array[Array[Int]] = Array(Array(0, 4, 8, 12), Array(1, 5, 9, 13), Array(2, 6, 10, 14), Array(3, 7, 11, 15))

scala> res12 map (_ map ("%2d" format _) mkString " ") mkString "\n"
res16: String =
 0  1  2  3
 4  5  6  7
 8  9 10 11
12 13 14 15

scala> res13 map (_ map ("%2d" format _) mkString " ") mkString "\n"
res17: String =
 0  4  8 12
 1  5  9 13
 2  6 10 14
 3  7 11 15
