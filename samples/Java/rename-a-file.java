import java.io.File;
public class FileRenameTest {
   public static boolean renameFile(String oldname, String newname) {
       // File (or directory) with old name
       File file = new File(oldname);

       // File (or directory) with new name
       File file2 = new File(newname);

       // Rename file (or directory)
       boolean success = file.renameTo(file2);
       return success;
   }
   public static void test(String type, String oldname, String newname) {
       System.out.println("The following " + type + " called " + oldname +
           ( renameFile(oldname, newname) ? " was renamed as " : " could not be renamed into ")
           + newname + "."
       );
   }
   public static void main(String args[]) {
        test("file", "input.txt", "output.txt");
        test("file", File.separator + "input.txt", File.separator + "output.txt");
        test("directory", "docs", "mydocs");
        test("directory", File.separator + "docs" + File.separator, File.separator + "mydocs" + File.separator);
   }
}
