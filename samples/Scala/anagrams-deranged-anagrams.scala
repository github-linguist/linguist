object DerangedAnagrams {

  /** Returns a map of anagrams keyed by the sorted characters */
  def groupAnagrams(words: Iterable[String]): Map[String, Set[String]] =
    words.foldLeft (Map[String, Set[String]]()) { (map, word) =>
      val sorted = word.sorted
      val entry = map.getOrElse(sorted, Set.empty)
      map + (sorted -> (entry + word))
    }

  /* Returns true if the pair of strings has no positions with the same
   * characters */
  def isDeranged(ss: (String, String)): Boolean =
    ss._1 zip ss._2 forall { case (c1, c2) => c1 != c2 }

  /* Returns pairwise combination of all Strings in the argument Iterable */
  def pairWords(as: Iterable[String]): Iterable[(String, String)] =
    if (as.size < 2) Seq() else (as.tail map (as.head -> _)) ++ pairWords(as.tail)

  /* Returns the contents of the argument URL as an Iterable[String], each
   * String is one line in the file */
  def readLines(url: String): Iterable[String] =
    io.Source.fromURL(url).getLines().toIterable

  val wordsURL = "http://www.puzzlers.org/pub/wordlists/unixdict.txt"

  def main(args: Array[String]): Unit = {
    val anagramMap = groupAnagrams(readLines(wordsURL))
    val derangedPairs = anagramMap.values flatMap (pairWords) filter (isDeranged)
    val (w1, w2) = derangedPairs maxBy (pair => pair._1.length)
    println("Longest deranged pair: "+w1+" and "+w2)
  }

}
