public class Roots {
    public interface Function {
	public double f(double x);
    }

    private static int sign(double x) {
	return (x < 0.0) ? -1 : (x > 0.0) ? 1 : 0;
    }

    public static void printRoots(Function f, double lowerBound,
				  double upperBound, double step) {
	double x = lowerBound, ox = x;
	double y = f.f(x), oy = y;
	int s = sign(y), os = s;

	for (; x <= upperBound ; x += step) {
	    s = sign(y = f.f(x));
	    if (s == 0) {
		System.out.println(x);
	    } else if (s != os) {
		double dx = x - ox;
		double dy = y - oy;
		double cx = x - dx * (y / dy);
		System.out.println("~" + cx);
	    }
	    ox = x; oy = y; os = s;
	}
    }

    public static void main(String[] args) {
	Function poly = new Function () {
	    public double f(double x) {
		return x*x*x - 3*x*x + 2*x;
	    }
	};
	printRoots(poly, -1.0, 4, 0.002);
    }
}
