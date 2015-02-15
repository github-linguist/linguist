public class QuadraticRoots {
    private static class Complex {
        double re, im;

        public Complex(double re, double im) {
            this.re = re;
            this.im = im;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == this) {return true;}
            if (!(obj instanceof Complex)) {return false;}
            Complex other = (Complex) obj;
            return (re == other.re) && (im == other.im);
        }

        @Override
        public String toString() {
            if (im == 0.0) {return String.format("%g", re);}
            if (re == 0.0) {return String.format("%gi", im);}
            return String.format("%g %c %gi", re,
                (im < 0.0 ? '-' : '+'), Math.abs(im));
        }
    }

    private static Complex[] quadraticRoots(double a, double b, double c) {
        Complex[] roots = new Complex[2];
        double d = b * b - 4.0 * a * c;  // discriminant
        double aa = a + a;

        if (d < 0.0) {
            double re = -b / aa;
            double im = Math.sqrt(-d) / aa;
            roots[0] = new Complex(re, im);
            roots[1] = new Complex(re, -im);
        } else if (b < 0.0) {
            // Avoid calculating -b - Math.sqrt(d), to avoid any
            // subtractive cancellation when it is near zero.
            double re = (-b + Math.sqrt(d)) / aa;
            roots[0] = new Complex(re, 0.0);
            roots[1] = new Complex(c / (a * re), 0.0);
        } else {
            // Avoid calculating -b + Math.sqrt(d).
            double re = (-b - Math.sqrt(d)) / aa;
            roots[1] = new Complex(re, 0.0);
            roots[0] = new Complex(c / (a * re), 0.0);
        }
        return roots;
    }

    public static void main(String[] args) {
        double[][] equations = {
            {1.0, 22.0, -1323.0},   // two distinct real roots
            {6.0, -23.0, 20.0},     //   with a != 1.0
            {1.0, -1.0e9, 1.0},     //   with one root near zero
            {1.0, 2.0, 1.0},        // one real root (double root)
            {1.0, 0.0, 1.0},        // two imaginary roots
            {1.0, 1.0, 1.0}         // two complex roots
        };
        for (int i = 0; i < equations.length; i++) {
            Complex[] roots = quadraticRoots(
                equations[i][0], equations[i][1], equations[i][2]);
            System.out.format("%na = %g   b = %g   c = %g%n",
                equations[i][0], equations[i][1], equations[i][2]);
            if (roots[0].equals(roots[1])) {
                System.out.format("X1,2 = %s%n", roots[0]);
            } else {
                System.out.format("X1 = %s%n", roots[0]);
                System.out.format("X2 = %s%n", roots[1]);
            }
        }
    }
}
