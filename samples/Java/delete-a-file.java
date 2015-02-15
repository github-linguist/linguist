import java.io.File;
public class FileDeleteTest {
   public static boolean deleteFile(String filename) {
       boolean exists = new File(filename).delete();
       return exists;
   }
   public static void test(String type, String filename) {
       System.out.println("The following " + type + " called " + filename +
           (deleteFile(filename) ? " was deleted." : " could not be deleted.")
       );
   }
   public static void main(String args[]) {
        test("file", "input.txt");
        test("file", File.seperator + "input.txt");
        test("directory", "docs");
        test("directory", File.seperator + "docs" + File.seperator);
   }
}
