import scala.language.postfixOps
import scala.language.implicitConversions

class Dot[T](v1: Seq[T])(implicit n: Numeric[T]) {
  import n._ // import * operator
  def dot(v2: Seq[T]) = {
    require(v1.size == v2.size)
    v1 zip v2 map Function.tupled(_ * _) sum
  }
}

object Main extends App {
  implicit def toDot[T: Numeric](v1: Seq[T]) = new Dot(v1)

  val v1 = List(1, 3, -5)
  val v2 = List(4, -2, -1)
  println(v1 dot v2)
}
