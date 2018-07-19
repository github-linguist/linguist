import scala.annotation.tailrec
def multiSplit(str:String, sep:Seq[String])={
   def findSep(index:Int)=sep find (str startsWith (_, index))
   @tailrec def nextSep(index:Int):(Int,Int)=
      if(index>str.size) (index, 0) else findSep(index) match {
         case Some(sep) => (index, sep.size)
         case _ => nextSep(index + 1)
      }
   def getParts(start:Int, pos:(Int,Int)):List[String]={
      val part=str slice (start, pos._1)
      if(pos._2==0) List(part) else part :: getParts(pos._1+pos._2, nextSep(pos._1+pos._2))
   }
   getParts(0, nextSep(0))
}

println(multiSplit("a!===b=!=c", Seq("!=", "==", "=")))
