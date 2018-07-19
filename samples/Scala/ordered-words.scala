val wordsAll = scala.io.Source.fromURL("http://www.puzzlers.org/pub/wordlists/unixdict.txt").getLines.toSeq

/**
 * Given a sequence of words return a sub-sequence of the
 * words that have characters in sorted order.
 */
def orderedWords( words:Seq[String] ) : Seq[(String)] = {

  def isOrdered( s:String ) : Boolean =
    (s.foldLeft( (true,'@') ){
      case ((false,_),_) => return false
      case ((true,prev),c) => ((prev <= c),c)
    })._1

  wordsAll.filter( isOrdered(_) ).toSeq
}

val ww = orderedWords( wordsAll ).sortBy( -_.length )

println( ww.takeWhile( _.length == ww.head.length ).mkString("\n") )
