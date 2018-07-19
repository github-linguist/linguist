import java.util.Arrays;
public class FD {
    public static void main(String args[]) {
        double[] a = {90, 47, 58, 29, 22, 32, 55, 5, 55, 73};
        System.out.println(Arrays.toString(dif(a, 1)));
        System.out.println(Arrays.toString(dif(a, 2)));
        System.out.println(Arrays.toString(dif(a, 9)));
        System.out.println(Arrays.toString(dif(a, 10)));      //let's test
        System.out.println(Arrays.toString(dif(a, 11)));
        System.out.println(Arrays.toString(dif(a, -1)));
        System.out.println(Arrays.toString(dif(a, 0)));
    }

    public static double[] dif(double[] a, int n) {
        if (n < 0)
            return null; // if the programmer was dumb

        for (int i = 0; i < n && a.length > 0; i++) {
            double[] b = new double[a.length - 1];
            for (int j = 0; j < b.length; j++){
                b[j] = a[j+1] - a[j];
            }
            a = b; //"recurse"
        }
        return a;
    }
}
