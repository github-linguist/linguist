import Stream._
import math._
import scala.collection.mutable.ListBuffer

object VampireNumbers extends App {
  val elapsed: (=> Unit) => Long = f => {val s = System.currentTimeMillis; f; (System.currentTimeMillis - s)/1000}

  val sexp = from(1, 2)   // stream of integer: 1,3,5,7, ...
  val rs: Stream[Int] => Stream[Pair[Long,Long]] = exps => Pair(pow(10,exps.head).toLong,(pow(10,exps.head)*10-1).toLong)#::rs(exps.tail)
  val srs = rs(sexp)   // stream of ranges: [10..99], [1000..9999], [100000..999999], ...
  val cs: Stream[Pair[Long,Long]] => Stream[Long] = rs => (rs.head._1 to rs.head._2).toStream#:::cs(rs.tail)
  val scs = cs(srs)   // stream of candidates: 10,11,..,99,1000,1001,..,9999, ...
  val it = scs.iterator

  val checkVN: Long => Pair[Long,Seq[Pair[Long,Long]]] = n => {
    val check: Pair[Long,Long] => Pair[Long,Long] = p => {
      val len: Long => Int = n => n.toString.size
      val (a,b) = p
      if ((a%10==0)&&(b%10==0)) Pair(0,0) else
      if (len(a) != len(b)) Pair(0,0) else
      if (n.toString.toList.diff(a.toString.toList++b.toString.toList)!=Nil) Pair(0,0) else p
    }
    Pair(n,(pow(10,log10(sqrt(n).toLong).toLong).toLong+1 to sqrt(n).toLong).filter{i=>n%i==0}
     .map {fac =>Pair(fac,n/fac)}.map {p => check(p)}.filter {p => p._1 != 0})
  }

  val et = elapsed {
    val lb = new ListBuffer[Pair[Long,Seq[Pair[Long,Long]]]]
    while ((lb.size<25)&&(it.hasNext)) {
      checkVN(it.next) match {
        case (n, Seq()) =>
        case p          => lb += p
      }
    }

    lb.toList.zipWithIndex.foreach {p =>
      println(p._2+1+": "+p._1._1+(p._1._2:\"")((x,y)=>" = "+x._1+" x "+x._2+y))
    }
    println

    List(16758243290880L, 24959017348650L, 14593825548650L)
      .map {checkVN(_)}
      .foreach {
         case (n, Seq()) => println(n+" is not vampiric")
         case p => println(p._1+(p._2:\"")((x,y)=>" = "+x._1+" x "+x._2+y))
       }
  }

  println("\n"+"elapsed time: "+et+" seconds")
}
