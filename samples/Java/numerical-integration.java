class NumericalIntegration
{

  interface FPFunction
  {
    double eval(double n);
  }

  public static double rectangularLeft(double a, double b, int n, FPFunction f)
  {
    return rectangular(a, b, n, f, 0);
  }

  public static double rectangularMidpoint(double a, double b, int n, FPFunction f)
  {
    return rectangular(a, b, n, f, 1);
  }

  public static double rectangularRight(double a, double b, int n, FPFunction f)
  {
    return rectangular(a, b, n, f, 2);
  }

  public static double trapezium(double a, double b, int n, FPFunction f)
  {
    double range = checkParamsGetRange(a, b, n);
    double nFloat = (double)n;
    double sum = 0.0;
    for (int i = 1; i < n; i++)
    {
      double x = a + range * (double)i / nFloat;
      sum += f.eval(x);
    }
    sum += (f.eval(a) + f.eval(b)) / 2.0;
    return sum * range / nFloat;
  }

  public static double simpsons(double a, double b, int n, FPFunction f)
  {
    double range = checkParamsGetRange(a, b, n);
    double nFloat = (double)n;
    double sum1 = f.eval(a + range / (nFloat * 2.0));
    double sum2 = 0.0;
    for (int i = 1; i < n; i++)
    {
      double x1 = a + range * ((double)i + 0.5) / nFloat;
      sum1 += f.eval(x1);
      double x2 = a + range * (double)i / nFloat;
      sum2 += f.eval(x2);
    }
    return (f.eval(a) + f.eval(b) + sum1 * 4.0 + sum2 * 2.0) * range / (nFloat * 6.0);
  }

  private static double rectangular(double a, double b, int n, FPFunction f, int mode)
  {
    double range = checkParamsGetRange(a, b, n);
    double modeOffset = (double)mode / 2.0;
    double nFloat = (double)n;
    double sum = 0.0;
    for (int i = 0; i < n; i++)
    {
      double x = a + range * ((double)i + modeOffset) / nFloat;
      sum += f.eval(x);
    }
    return sum * range / nFloat;
  }

  private static double checkParamsGetRange(double a, double b, int n)
  {
    if (n <= 0)
      throw new IllegalArgumentException("Invalid value of n");
    double range = b - a;
    if (range <= 0)
      throw new IllegalArgumentException("Invalid range");
    return range;
  }


  private static void testFunction(String fname, double a, double b, int n, FPFunction f)
  {
    System.out.println("Testing function \"" + fname + "\", a=" + a + ", b=" + b + ", n=" + n);
    System.out.println("rectangularLeft: " + rectangularLeft(a, b, n, f));
    System.out.println("rectangularMidpoint: " + rectangularMidpoint(a, b, n, f));
    System.out.println("rectangularRight: " + rectangularRight(a, b, n, f));
    System.out.println("trapezium: " + trapezium(a, b, n, f));
    System.out.println("simpsons: " + simpsons(a, b, n, f));
    System.out.println();
    return;
  }

  public static void main(String[] args)
  {
    testFunction("x^3", 0.0, 1.0, 100, new FPFunction() {
        public double eval(double n) {
          return n * n * n;
        }
      }
    );

    testFunction("1/x", 1.0, 100.0, 1000, new FPFunction() {
        public double eval(double n) {
          return 1.0 / n;
        }
      }
    );

    testFunction("x", 0.0, 5000.0, 5000000, new FPFunction() {
        public double eval(double n) {
          return n;
        }
      }
    );

    testFunction("x", 0.0, 6000.0, 6000000, new FPFunction() {
        public double eval(double n) {
          return n;
        }
      }
    );

    return;
  }
}
