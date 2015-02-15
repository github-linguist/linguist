import java.io.*;

public class StripBlockComments{
    public static String readFile(String filename) {
	BufferedReader reader = new BufferedReader(new FileReader(filename));
	try {
	    StringBuilder fileContents = new StringBuilder();
	    char[] buffer = new char[4096];
	    while (reader.read(buffer, 0, 4096) > 0) {
		fileContents.append(buffer);
	    }
	    return fileContents.toString();
	} finally {
	    reader.close();
	}
    }

    public static String stripComments(String beginToken, String endToken,
				       String input) {
	StringBuilder output = new StringBuilder();
	while (true) {
	    int begin = input.indexOf(beginToken);
	    int end = input.indexOf(endToken, begin+beginToken.length());
	    if (begin == -1 || end == -1) {
		output.append(input);
		return output.toString();
	    }
	    output.append(input.substring(0, begin));
	    input = input.substring(end + endToken.length());
	}
    }

    public static void main(String[] args) {
	if (args.length < 3) {
	    System.out.println("Usage: BeginToken EndToken FileToProcess");
	    System.exit(1);
	}

	String begin = args[0];
	String end = args[1];
	String input = args[2];

	try {
	    System.out.println(stripComments(begin, end, readFile(input)));
	} catch (Exception e) {
	    e.printStackTrace();
	    System.exit(1);
	}
    }
}
