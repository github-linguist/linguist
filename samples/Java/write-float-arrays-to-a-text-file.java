import java.io.*;

public class FloatArray {
    public static void writeDat(String filename, double[] x, double[] y,
                                int xprecision, int yprecision)
        throws IOException {
        assert x.length == y.length;
        PrintWriter out = new PrintWriter(filename);
        for (int i = 0; i < x.length; i++)
            out.printf("%."+xprecision+"g\t%."+yprecision+"g\n", x[i], y[i]);
        out.close();
    }

    public static void main(String[] args) {
        double[] x = {1, 2, 3, 1e11};
        double[] y = new double[x.length];
        for (int i = 0; i < x.length; i++)
            y[i] = Math.sqrt(x[i]);

        try {
            writeDat("sqrt.dat", x, y, 3, 5);
        } catch (IOException e) {
            System.err.println("writeDat: exception: "+e);
        }

        try {
            BufferedReader br = new BufferedReader(new FileReader("sqrt.dat"));
            String line;
            while ((line = br.readLine()) != null)
                System.out.println(line);
        } catch (IOException e) { }
    }
}
