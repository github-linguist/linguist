import scala.util.control.Breaks._
val a=Array.fill(5,4)(scala.util.Random.nextInt(21))
println(a map (_.mkString("[", ", ", "]")) mkString "\n")
breakable {
  for(row <- a; x <- row){
    println(x)
    if (x==20) break
  }
}
