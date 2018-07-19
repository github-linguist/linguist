package linenbr7;

import java.io.*;

public class LineNbr7 {

    public static void main(String[] args) throws Exception {
        File f = new File(args[0]);
        if (!f.isFile() || !f.canRead())
            throw new IOException("can't read " + args[0]);

        BufferedReader br = new BufferedReader(new FileReader(f));
        try (LineNumberReader lnr = new LineNumberReader(br)) {
            String line = null;
            int lnum = 0;
            while ((line = lnr.readLine()) != null
                    && (lnum = lnr.getLineNumber()) < 7) {
            }

            switch (lnum) {
                case 0:
                    System.out.println("the file has zero length");
                    break;
                case 7:
                    boolean empty = "".equals(line);
                    System.out.println("line 7: " + (empty ? "empty" : line));
                    break;
                default:
                    System.out.println("the file has only " + lnum + " line(s)");
            }
        }
    }
}
