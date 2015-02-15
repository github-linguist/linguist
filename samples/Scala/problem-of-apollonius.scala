object ApolloniusSolver extends App {
  case class Circle(x: Double, y: Double, r: Double)
  object Tangent extends Enumeration {
	type Tangent = Value
	val intern = Value(-1)
	val extern = Value(1)
  }

  import Tangent._
  import scala.Math._

  val solveApollonius: (Circle, Circle, Circle, Triple[Tangent, Tangent, Tangent]) => Circle = (c1, c2, c3, tangents) => {
    val fv: (Circle, Circle, Int, Int) => Tuple4[Double, Double, Double, Double] = (c1, c2, s1, s2) => {
      val v11 = 2 * c2.x - 2 * c1.x
      val v12 = 2 * c2.y - 2 * c1.y
      val v13 = pow(c1.x, 2) - pow(c2.x, 2) + pow(c1.y, 2) - pow(c2.y, 2) - pow(c1.r, 2) + pow(c2.r, 2)
      val v14 = 2 * s2 * c2.r - 2 * s1 * c1.r
      Tuple4(v11, v12, v13, v14)
    }
    val (s1, s2, s3) = (tangents._1.id, tangents._2.id, tangents._3.id)

    val (v11, v12, v13, v14) = fv(c1, c2, s1, s2)
    val (v21, v22, v23, v24) = fv(c2, c3, s2, s3)

    val w12 = v12 / v11
    val w13 = v13 / v11
    val w14 = v14 / v11

    val w22 = v22 / v21 - w12
    val w23 = v23 / v21 - w13
    val w24 = v24 / v21 - w14

    val P = -w23 / w22
    val Q =  w24 / w22
    val M = -w12 * P - w13
    val N =  w14 - w12 * Q

    val a = N*N + Q*Q - 1
    val b = 2*M*N - 2*N*c1.x +
            2*P*Q - 2*Q*c1.y +
            2*s1*c1.r
    val c = pow(c1.x, 2) + M*M - 2*M*c1.x +
            P*P + pow(c1.y, 2) - 2*P*c1.y - pow(c1.r, 2)

    // Find a root of a quadratic equation. This requires the circle centers not to be e.g. colinear
    val D = b*b - 4*a*c
    val rs = (-b - sqrt(D)) / (2*a)

    Circle(x=M + N*rs, y=P + Q*rs, r=rs)
  }	

  val c1 = Circle(x=0.0, y=0.0, r=1.0)
  val c2 = Circle(x=4.0, y=0.0, r=1.0)
  val c3 = Circle(x=2.0, y=4.0, r=2.0)

  println("c1: "+c1)
  println("c2: "+c2)
  println("c3: "+c3)

  println{
    val tangents = Triple(intern, intern, intern)
    "red circle:   tangents="+tangents+" cs=" + solveApollonius(c1, c2, c3, tangents)
  }
  println{
    val tangents = Triple(extern, extern, extern)
    "green circle: tangents="+tangents+" cs=" + solveApollonius(c1, c2, c3, tangents)
  }

  println("all combinations:")
  for ( ti <- Tangent.values)
    for ( tj <- Tangent.values)
      for ( tk <- Tangent.values) {
        println{
          val format: Circle => String = c => {
            "Circle(x=%8.5f, y=%8.5f, r=%8.5f)".format(c.x, c.y, c.r)
          }
          val tangents = Triple(ti, tj, tk)
          "tangents: " + tangents + " -> cs=" + format(solveApollonius(c1, c2, c3, tangents))
        }
      }
}
