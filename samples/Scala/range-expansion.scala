def rangex(str: String): Seq[Int] =
  str split "," flatMap { (s) =>
    val r = """(-?\d+)(?:-(-?\d+))?""".r
    val r(a,b) = s
    if (b == null) Seq(a.toInt) else a.toInt to b.toInt
  }
