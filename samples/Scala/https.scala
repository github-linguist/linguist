import scala.io.Source

object HttpsTest extends App {
  System.setProperty("http.agent", "*")

  Source.fromURL("https://sourceforge.net").getLines.foreach(println)
}
