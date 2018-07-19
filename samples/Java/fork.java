import java.io.IOException;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

public class RFork {

  public static void main(String[] args) {
    ProcessBuilder pb;
    Process pp;
    List<String> command;
    Map<String, String> env;
    BufferedReader ir;
    String currentuser;
    String line;
    try {
      command = Arrays.asList("");
      pb = new ProcessBuilder(command);
      env = pb.environment();
      currentuser = env.get("USER");
      command = Arrays.asList("ps", "-f", "-U", currentuser);
      pb.command(command);
      pp = pb.start();
      ir = new BufferedReader(new InputStreamReader(pp.getInputStream()));
      line = "Output of running " + command.toString() + " is:";
      do {
        System.out.println(line);
      } while ((line = ir.readLine()) != null);
    }
    catch (IOException iox) {
      iox.printStackTrace();
    }

    return;
  }
}
