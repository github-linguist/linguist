import java.io.{ FileNotFoundException, PrintWriter }

object FileIO extends App {
  try {
    val MyFileTxtSource = scala.io.Source.fromFile("input.txt")
    val MyFileTxtTarget = new PrintWriter("output.txt")

    val str = MyFileTxtSource.mkString
    MyFileTxtTarget.print(str)

    MyFileTxtTarget.close()
    MyFileTxtSource.close()
    }
  } catch {
    case e: FileNotFoundException => println(e.getLocalizedMessage())
    case e: Throwable => {
      println("Some other exception type:")
      e.printStackTrace()
    }
  }
}
