import java.io.{ FileNotFoundException, FileOutputStream, PrintStream }
import java.util.Date

object TakeNotes extends App {
  val notesFileName = "notes.txt"
  if (args.length > 0) {
    val ps = new PrintStream(new FileOutputStream(notesFileName, true))
    ps.println(new Date() + args.mkString("\n\t", " ", "."))
    ps.close()
  } else try {
    io.Source.fromFile(notesFileName).getLines().foreach { line => println(line) }
  } catch {
    case e: FileNotFoundException => println(e.getLocalizedMessage())
    case e: Throwable => {
      println("Some other exception type:")
      e.printStackTrace()
    }
  }
}
