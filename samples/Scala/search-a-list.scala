def findNeedles(needle: String, haystack: Seq[String]) = haystack.zipWithIndex.filter(_._1 == needle).map(_._2)
def firstNeedle(needle: String, haystack: Seq[String]) = findNeedles(needle, haystack).head
def lastNeedle(needle: String, haystack: Seq[String]) = findNeedles(needle, haystack).last
