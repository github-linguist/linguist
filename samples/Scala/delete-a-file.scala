import java.util._
import java.io.File

object FileDeleteTest extends App {

  def deleteFile(filename: String) = {
    new File(filename).delete();
  }

  def test(typ: String, filename: String) = {
    System.out.println("The following " + typ + " called " + filename +
      (if (deleteFile(filename)) " was deleted." else " could not be deleted."))
  }
  test("file", "input.txt");
  test("file", File.separatorChar + "input.txt");
  test("directory", "docs");
  test("directory", File.separatorChar + "docs" + File.separatorChar);
}
