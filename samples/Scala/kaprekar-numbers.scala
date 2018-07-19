object Kaprekar extends App {

  def isKaprekar(n: Int, base: Int = 10):Option[Triple[String,String,String]] = {
    val check: Long => Option[Triple[String,String,String]] = n => {
      val split: Pair[String, Int] => Pair[String, String] = p => (p._1.slice(0,p._2),p._1.slice(p._2,p._1.size).padTo[Char,String](1,'0'))
      val pwr = n*n
      val sN = java.lang.Long.toString(n, base)
      val sPwr = java.lang.Long.toString(pwr, base)
      for (i <- 1 to sPwr.size) {
        val (a, b) = split(sPwr,i)
        val la = java.lang.Long.parseLong(a, base)
        val lb = java.lang.Long.parseLong(b, base)
        if (lb==0) return None
        if (la+lb==n) return Some(Triple(sPwr,a,b))
      }
      None
    }
    n match {
      case 1 => Some(Triple("1","0","1"))
      case n if (n>1) => check(n)
      case _ => None
    }
  }

  def kaprekars(n: Int,base: Int=10) = (1 to n).map(isKaprekar(_,base)).zip(1 to n).filter(_._1!=None).map(p=>Triple(base,p._2,p._1 match {case Some(t) => t; case _ => Nil}))

  val k1 = kaprekars(10000)
  k1 foreach {p=>println(p._2)}
  println(k1.size + " Kaprekar numbers < 10000 (b:10) for base 10"+"\n"*2)

  val k2 = kaprekars(1000000)
  k2 foreach {p => println(p._2+"\t"+java.lang.Long.toString(p._2,p._1)+"\t"+p._3.productElement(0)+"\t"+p._3.productElement(1)+" + "+p._3.productElement(2))}
  println(k2.size + " Kaprekar numbers < 1000000 (b:10) for base 10"+"\n"*2)

  val k3 = kaprekars(1000000,17)
  k3 foreach {p => println(p._2+"\t"+java.lang.Long.toString(p._2,p._1)+"\t"+p._3.productElement(0)+"\t"+p._3.productElement(1)+" + "+p._3.productElement(2))}
  println(k3.size + " Kaprekar numbers < 1000000 (b:10) for base 17"+"\n"*2)

}
