import java.io.PrintStream;
import java.io.UnsupportedEncodingException;

public class Main
{
    public static void main(String[] args) throws UnsupportedEncodingException
    {
        PrintStream writer = new PrintStream(System.out, true, "UTF-8");
        writer.println("£");
        writer.println("札幌");
    }
}
