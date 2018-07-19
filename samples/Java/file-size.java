import java.io.File;

public class FileSize
{
    public static void main ( String[] args )
    {
        System.out.println("input.txt  : " + new File("input.txt").length() + " bytes");
        System.out.println("/input.txt : " + new File("/input.txt").length() + " bytes");
    }
}
