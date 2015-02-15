import java.io.File

try {
    // Create temp file
    val filename = File.createTempFile("prefix", ".suffix")

    // Delete temp file when program exits
    filename.deleteOnExit

    System.out.println(filename)

} catch {
  case _: java.io.IOException =>
}
