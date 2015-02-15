scala> def isHappy(n: Int) = {
     |   new Iterator[Int] {
     |   val seen = scala.collection.mutable.Set[Int]()
     |   var curr = n
     |   def next = {
     |     val res = curr
     |     curr = res.toString.map(_.asDigit).map(n => n * n).sum
     |     seen += res
     |     res
     |   }
     |   def hasNext = !seen.contains(curr)
     | }.toList.last == 1
     | }
isHappy: (n: Int)Boolean

scala> Iterator from 1 filter isHappy take 8 foreach println
1
7
10
13
19
23
28
31
