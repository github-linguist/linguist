import java.io.File;
import java.util.Date;
public class FileModificationTimeTest {
   public static void test(String type, File file) {
       long t = file.lastModified();
       System.out.println("The following " + type + " called " + file.getPath() +
            (t == 0 ? " does not exist." : " was modified at " + new Date(t).toString() )
       );
       System.out.println("The following " + type + " called " + file.getPath() +
            (!file.setLastModified(System.currentTimeMillis()) ? " does not exist." : " was modified to current time." )
       );
       System.out.println("The following " + type + " called " + file.getPath() +
            (!file.setLastModified(t) ? " does not exist." : " was modified to previous time." )
       );
   }
   public static void main(String args[]) {
       test("file", new File("output.txt"));
       test("directory", new File("docs"));
   }
}
