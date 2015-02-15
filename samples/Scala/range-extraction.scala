object Range {
   def spanRange(ls:List[Int])={
     var last=ls.head
     ls span {x => val b=x<=last+1; last=x; b}
   }

   def toRangeList(ls:List[Int]):List[List[Int]]=ls match {
      case Nil => List()
      case _ => spanRange(ls) match {
         case (range, Nil) => List(range)
         case (range, rest) => range :: toRangeList(rest)
      }
   }

   def toRangeString(ls:List[List[Int]])=ls map {r=>
      if(r.size<3) r mkString ","
      else r.head + "-" + r.last
   } mkString ","

   def main(args: Array[String]): Unit = {
      var l=List(0, 1, 2, 4, 6, 7, 8, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
                 27, 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39)
      println(toRangeString(toRangeList(l)))
   }
}
