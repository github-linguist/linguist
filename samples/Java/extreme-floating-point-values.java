public class Extreme {
    public static void main(String[] args) {
        double negInf = -1.0 / 0.0; //also Double.NEGATIVE_INFINITY
        double inf = 1.0 / 0.0; //also Double.POSITIVE_INFINITY
        double nan = 0.0 / 0.0; //also Double.NaN
        double negZero = -2.0 / inf;

        System.out.println("Negative inf: " + negInf);
        System.out.println("Positive inf: " + inf);
        System.out.println("NaN: " + nan);
        System.out.println("Negative 0: " + negZero);
        System.out.println("inf + -inf: " + (inf + negInf));
        System.out.println("0 * NaN: " + (0 * nan));
        System.out.println("NaN == NaN: " + (nan == nan));
    }
}
