import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;

public class Main
{
    public static void main(String[] args) throws UnsupportedEncodingException
    {
        String encoded = "http%3A%2F%2Ffoo%20bar%2F";
        String normal = URLDecoder.decode(encoded, "utf-8");
        System.out.println(normal);
    }
}
