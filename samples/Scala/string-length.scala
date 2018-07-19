object StringLength extends App {
  val s1 = "møøse"
  val s3 = List("\uD835\uDD18", "\uD835\uDD2B", "\uD835\uDD26",
    "\uD835\uDD20", "\uD835\uDD2C", "\uD835\uDD21", "\uD835\uDD22").mkString
  val s4 = "J\u0332o\u0332s\u0332e\u0301\u0332"

    List(s1, s3, s4).foreach(s => println(
        s"The string: $s, characterlength= ${s.length} UTF8bytes= ${
      s.getBytes("UTF-8").size
    } UTF16bytes= ${s.getBytes("UTF-16LE").size}"))
}
