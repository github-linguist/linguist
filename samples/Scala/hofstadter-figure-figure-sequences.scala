object HofstadterFigFigSeq extends App {
  import scala.collection.mutable.ListBuffer

  val r = ListBuffer(0, 1)
  val s = ListBuffer(0, 2)

  def ffr(n: Int): Int = {
    val ffri: Int => Unit = i => {
      val nrk = r.size - 1
      val rNext = r(nrk)+s(nrk)
      r += rNext
      (r(nrk)+2 to rNext-1).foreach{s += _}
      s += rNext+1
    }

    (r.size to n).foreach(ffri(_))
    r(n)
  }

  def ffs(n:Int): Int = {
    while (s.size <= n) ffr(r.size)
    s(n)
  }

  (1 to 10).map(i=>(i,ffr(i))).foreach(t=>println("r("+t._1+"): "+t._2))
  println((1 to 1000).toList.filterNot(((1 to 40).map(ffr(_))++(1 to 960).map(ffs(_))).contains)==List())
}
