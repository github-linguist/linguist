import java.io.FileWriter;
import java.io.IOException;

public class LinePrinter {
  public static void main(String[] args) {
    try {
      FileWriter lp0 = new FileWriter("/dev/lp0");
      lp0.write("Hello World!");
      lp0.close();
    } catch (IOException ioe) {
      ioe.printStackTrace();
    }
  }
}
