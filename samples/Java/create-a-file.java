import java.io.*;
public class CreateFileTest {
	public static void main(String args[]) {
		try {
			new File("output.txt").createNewFile();
			new File(File.separator + "output.txt").createNewFile();
			new File("docs").mkdir();
			new File(File.separator + "docs").mkdir();
		} catch (IOException e) {
			System.err.println(e.getMessage());
		}
	}
}
