import java.io.*;
import java.nio.channels.*;
import java.util.Date;

public class TakeNotes {
    public static void main(String[] args) throws IOException {
        if (args.length > 0) {
            PrintStream ps = new PrintStream(new FileOutputStream("notes.txt", true));
            ps.println(new Date());
            ps.print("\t" + args[0]);
            for (int i = 1; i < args.length; i++)
                ps.print(" " + args[i]);
            ps.println();
            ps.close();
        } else {
            FileChannel fc = new FileInputStream("notes.txt").getChannel();
            fc.transferTo(0, fc.size(), Channels.newChannel(System.out));
            fc.close();
        }
    }
}
