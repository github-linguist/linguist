object TextFileSlurper extends App {
  val fileLines =
    try scala.io.Source.fromFile("my_file.txt", "UTF-8").mkString catch {
      case e: java.io.FileNotFoundException => ""
    }
}
