import java.io.File

object `package` {
  def walkTree(file: File): Iterable[File] = {
    val children = new Iterable[File] {
      def iterator = if (file.isDirectory) file.listFiles.iterator else Iterator.empty
    }
    Seq(file) ++: children.flatMap(walkTree(_))
  }
}

object Test extends App {
  val dir = new File("/home/user")
  for(f <- walkTree(dir)) println(f)
  for(f <- walkTree(dir) if f.getName.endsWith(".mp3")) println(f)
}
