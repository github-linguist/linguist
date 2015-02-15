public class Quaternion {
    private final double a, b, c, d;

    public Quaternion(double a, double b, double c, double d) {
        this.a = a;
        this.b = b;
        this.c = c;
        this.d = d;
    }
    public Quaternion(double r) {
        this(r, 0.0, 0.0, 0.0);
    }

    public double norm() {
        return Math.sqrt(a * a + b * b + c * c + d * d);
    }

    public Quaternion negative() {
        return new Quaternion(-a, -b, -c, -d);
    }

    public Quaternion conjugate() {
        return new Quaternion(a, -b, -c, -d);
    }

    public Quaternion add(double r) {
        return new Quaternion(a + r, b, c, d);
    }
    public static Quaternion add(Quaternion q, double r) {
        return q.add(r);
    }
    public static Quaternion add(double r, Quaternion q) {
        return q.add(r);
    }
    public Quaternion add(Quaternion q) {
        return new Quaternion(a + q.a, b + q.b, c + q.c, d + q.d);
    }
    public static Quaternion add(Quaternion q1, Quaternion q2) {
        return q1.add(q2);
    }

    public Quaternion times(double r) {
        return new Quaternion(a * r, b * r, c * r, d * r);
    }
    public static Quaternion times(Quaternion q, double r) {
        return q.times(r);
    }
    public static Quaternion times(double r, Quaternion q) {
        return q.times(r);
    }
    public Quaternion times(Quaternion q) {
        return new Quaternion(
            a * q.a - b * q.b - c * q.c - d * q.d,
            a * q.b + b * q.a + c * q.d - d * q.c,
            a * q.c - b * q.d + c * q.a + d * q.b,
            a * q.d + b * q.c - c * q.b + d * q.a
        );
    }
    public static Quaternion times(Quaternion q1, Quaternion q2) {
        return q1.times(q2);
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof Quaternion)) return false;
        final Quaternion other = (Quaternion) obj;
        if (Double.doubleToLongBits(this.a) != Double.doubleToLongBits(other.a)) return false;
        if (Double.doubleToLongBits(this.b) != Double.doubleToLongBits(other.b)) return false;
        if (Double.doubleToLongBits(this.c) != Double.doubleToLongBits(other.c)) return false;
        if (Double.doubleToLongBits(this.d) != Double.doubleToLongBits(other.d)) return false;
        return true;
    }
    @Override
    public String toString() {
        return String.format("%.2f + %.2fi + %.2fj + %.2fk", a, b, c, d).replaceAll("\\+ -", "- ");
    }

    public String toQuadruple() {
        return String.format("(%.2f, %.2f, %.2f, %.2f)", a, b, c, d);
    }

    public static void main(String[] args) {
        Quaternion q = new Quaternion(1.0, 2.0, 3.0, 4.0);
        Quaternion q1 = new Quaternion(2.0, 3.0, 4.0, 5.0);
        Quaternion q2 = new Quaternion(3.0, 4.0, 5.0, 6.0);
        double r = 7.0;
        System.out.format("q       = %s%n", q);
        System.out.format("q1      = %s%n", q1);
        System.out.format("q2      = %s%n", q2);
        System.out.format("r       = %.2f%n%n", r);
        System.out.format("\u2016q\u2016     = %.2f%n", q.norm());
        System.out.format("-q      = %s%n", q.negative());
        System.out.format("q*      = %s%n", q.conjugate());
        System.out.format("q + r   = %s%n", q.add(r));
        System.out.format("q1 + q2 = %s%n", q1.add(q2));
        System.out.format("q \u00d7 r   = %s%n", q.times(r));
        Quaternion q1q2 = q1.times(q2);
        Quaternion q2q1 = q2.times(q1);
        System.out.format("q1 \u00d7 q2 = %s%n", q1q2);
        System.out.format("q2 \u00d7 q1 = %s%n", q2q1);
        System.out.format("q1 \u00d7 q2 %s q2 \u00d7 q1%n", (q1q2.equals(q2q1) ? "=" : "\u2260"));
    }
}
