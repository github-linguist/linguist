object BoxingTheCompass extends App {
  val cardinal = List("north", "east", "south", "west")
  val pointDesc = List("1", "1 by 2", "1-C", "C by 1", "C", "C by 2", "2-C", "2 by 1")

  val pointDeg: Int => Double = i => {
	val fswitch: Int => Int = i => i match {case 1 => 1; case 2 => -1; case _ => 0}
	i*11.25+fswitch(i%3)*5.62
  }

  val deg2ind: Double => Int = deg => (deg*32/360+.5).toInt%32+1

  val pointName: Int => String = ind => {
    val i = ind - 1
    val str1 = cardinal(i%32/8)
    val str2 = cardinal((i%32/8+1)%4)
    val strC = if ((str1 == "north") || (str1 == "south")) str1+str2 else str2+str1
    pointDesc(i%32%8).replace("1", str1).replace("2", str2).replace("C", strC).capitalize
  }

  (0 to 32).map(i=>Triple(pointDeg(i),deg2ind(pointDeg(i)),pointName(deg2ind(pointDeg(i)))))
      .map{t=>(printf("%s\t%18s\t%s°\n",t._2,t._3,t._1))}
}
