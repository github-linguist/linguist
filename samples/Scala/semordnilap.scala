val wordsAll = scala.io.Source.fromURL("http://www.puzzlers.org/pub/wordlists/unixdict.txt").getLines.map(_.toLowerCase).to[IndexedSeq]

/**
 * Given a sequence of lower-case words return a sub-sequence
 * of matches containing the word and its reverse if the two
 * words are different.
 */
def semordnilap( words:Seq[String] ) : Seq[(String,String)] = {

  ( words.
    zipWithIndex.                        // index will be needed to eliminate duplicate
    filter {
      case (w,i) =>
        val j = words.indexOf(w.reverse) // eg. (able,62) and (elba,7519)
        i < j && w != w.reverse          // save the matches which are not palindromes
    }
  ).
  map {
    case (w,i) => (w,w.reverse)          // drop the index
  }
}

val ss = semordnilap(wordsAll)

{
println( ss.size + " matches, including: \n" )
println( ss.take(5).mkString( "\n" ) )
}
