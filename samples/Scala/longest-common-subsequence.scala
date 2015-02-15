object LCS extends App {

  // recursive version:
  def lcsr(a: String, b: String): String = {
    if (a.size==0 || b.size==0) ""
    else if (a==b) a
      else
        if(a(a.size-1)==b(b.size-1)) lcsr(a.substring(0,a.size-1),b.substring(0,b.size-1))+a(a.size-1)
        else {
          val x = lcsr(a,b.substring(0,b.size-1))
          val y = lcsr(a.substring(0,a.size-1),b)
          if (x.size > y.size) x else y
        }
  }

  // dynamic programming version:
  def lcsd(a: String, b: String): String = {
    if (a.size==0 || b.size==0) ""
    else if (a==b) a
      else {
        val lengths = Array.ofDim[Int](a.size+1,b.size+1)
        for (i <- 0 until a.size)
          for (j <- 0 until b.size)
            if (a(i) == b(j))
              lengths(i+1)(j+1) = lengths(i)(j) + 1
            else
              lengths(i+1)(j+1) = scala.math.max(lengths(i+1)(j),lengths(i)(j+1))

        // read the substring out from the matrix
        val sb = new StringBuilder()
        var x = a.size
        var y = b.size
        do {
          if (lengths(x)(y) == lengths(x-1)(y))
            x -= 1
          else if (lengths(x)(y) == lengths(x)(y-1))
            y -= 1
          else {
            assert(a(x-1) == b(y-1))
            sb += a(x-1)
            x -= 1
            y -= 1
          }
        } while (x!=0 && y!=0)
        sb.toString.reverse
      }
  }

  val elapsed: (=> Unit) => Long = f => {val s = System.currentTimeMillis; f; (System.currentTimeMillis - s)/1000}

  val pairs = List(("thisiaatest","testing123testing")
                  ,("","x")
                  ,("x","x")
                  ,("beginning-middle-ending", "beginning-diddle-dum-ending"))

  var s = ""
  println("recursive version:")
  pairs foreach {p =>
    println{val t = elapsed(s = lcsr(p._1,p._2))
            "lcsr(\""+p._1+"\",\""+p._2+"\") = \""+s+"\"   ("+t+" sec)"}
  }

  println("\n"+"dynamic programming version:")
  pairs foreach {p =>
    println{val t = elapsed(s = lcsd(p._1,p._2))
            "lcsd(\""+p._1+"\",\""+p._2+"\") = \""+s+"\"   ("+t+" sec)"}
  }

}
