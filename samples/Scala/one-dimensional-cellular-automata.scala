def cellularAutomata(s: String) = {
  def it = Iterator.iterate(s) ( generation =>
    ("_%s_" format generation).iterator
    sliding 3
    map (_ count (_ == '#'))
    map Map(2 -> "#").withDefaultValue("_")
    mkString
  )

  (it drop 1) zip it takeWhile Function.tupled(_ != _) map (_._2) foreach println
}
