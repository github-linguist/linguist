scala> import util.control.Breaks.{breakable, break}
import util.control.Breaks.{breakable, break}

scala> import util.Random
import util.Random

scala> breakable {
     |   while(true) {
     |     val a = Random.nextInt(20)
     |     println(a)
     |     if(a == 10)
     |       break
     |     val b = Random.nextInt(20)
     |     println(b)
     |   }
     | }
5
4
10
