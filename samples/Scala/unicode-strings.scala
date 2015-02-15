object UTF8 extends App {

  def charToInt(s: String) = {
    def charToInt0(c: Char, next: Char): Option[Int] = (c, next) match {
      case _ if (c.isHighSurrogate && next.isLowSurrogate) =>
        Some(java.lang.Character.toCodePoint(c, next))
      case _ if (c.isLowSurrogate) => None
      case _                       => Some(c.toInt)
    }

    if (s.length > 1) charToInt0(s(0), s(1)) else Some(s.toInt)
  }

  def intToChars(n: Int) = java.lang.Character.toChars(n).mkString

  println('\uD869'.isHighSurrogate + " " + '\uDEA5'.isLowSurrogate)

  println(charToInt("\uD869\uDEA5"))

  val b = "\uD869\uDEA5"
  println(b)

  val c = "\uD834\uDD1E"
  println(c)

  val a = "$abcde¢£¤¥©ÇßçĲĳŁłʒλπ•₠₡₢₣₤₥₦₧₨₩₪₫€₭₮₯₰₱₲₳₴₵₵←→⇒∙⌘☺☻ア字文𪚥".
    map(c => "%s\t\\u%04X".format(c, c.toInt)).foreach(println)
}
