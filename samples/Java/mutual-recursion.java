public static int f(final int n)
{
 return n == 0 ? 1 : n - m(f(n - 1));
}

public static int m(final int n)
{
  return n == 0 ? 0 : n - f(m(n - 1));
}

public static void main(final String args[])
{
 for (int i = 0; i < 20; i++)
  System.out.println(f(i));
 System.out.println();
 for (i = 0; i < 20; i++)
  System.out.println(m(i));
}
