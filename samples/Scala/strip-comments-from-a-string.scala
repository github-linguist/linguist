object StripComments {
  def stripComments1(s:String, markers:String =";#")=s takeWhile (!markers.contains(_)) trim

  // using regex and pattern matching
  def stripComments2(s:String, markers:String =";#")={
    val R=("(.*?)[" + markers + "].*").r
    (s match {
      case R(line) => line
      case _ => s
    }) trim
  }

  def print(s:String)={
    println("'"+s+"' =>")
    println("   '"+stripComments1(s)+"'")
    println("   '"+stripComments2(s)+"'")
  }

  def main(args: Array[String]): Unit = {
    print("apples, pears # and bananas")
    print("apples, pears ; and bananas")
  }
}
