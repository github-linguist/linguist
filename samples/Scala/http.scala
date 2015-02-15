import scala.io.Source

object HttpTest extends App {
  System.setProperty("http.agent", "*")

  Source.fromURL("http://www.rosettacode.org").getLines.foreach(println)
}
