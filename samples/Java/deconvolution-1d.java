import java.util.Arrays;

public class Deconvolution1D {
    public static double[] deconv(double[] f, double[] g) {
        double[] h = new double[g.length - f.length + 1];
        for (int n = 0; n < h.length; n++) {
            h[n] = g[n];
            int lower = Math.max(n - f.length + 1, 0);
            for (int i = lower; i < n; i++)
                h[n] -= h[i] * f[n-i];
            h[n] /= f[0];
        }
        return h;
    }

    public static void main(String[] args) {
        double[] h = {-8, -9, -3, -1, -6, 7};
        double[] f = {-3, -6, -1, 8, -6, 3, -1, -9, -9, 3, -2, 5, 2, -2, -7, -1};
        double[] g = {24, 75, 71, -34, 3, 22, -45, 23, 245, 25, 52, 25, -67, -96,
            96, 31, 55, 36, 29, -43, -7};
        System.out.println(Arrays.toString(h));
        System.out.println(Arrays.toString(deconv(g, f)));
        System.out.println(Arrays.toString(f));
        System.out.println(Arrays.toString(deconv(g, h)));
    }
}
