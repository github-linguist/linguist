object Environment_variables extends App {

  val variablesToUse = List("PATH", "HOME", "HOMEPATH", "USERNAME")
  println((for {
    y <- sys.env
    x <- variablesToUse
    if y._1 == x
  } yield y).sorted.toMap.mkString("\n"))
}
