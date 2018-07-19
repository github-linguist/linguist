public class HofCon
{
 public static void main(final String[] args)
 {
  doSqnc(1<<20);
 }
 public static void doSqnc(int m)
 {
  int[] a_list = new int[m + 1];
  int max_df = 0;
  int p2_max = 2;
  int k1 = 2;
  int lg2 = 1;
  double amax = 0;
  a_list[0] = a_list[1] = 1;
  int v = a_list[2];
  for (int n = 2; n <= m; n++)
  {
   v = a_list[n] = a_list[v] + a_list[n - v];
   if (amax < v * 1.0 / n)
    amax = v * 1.0 / n;
   if (0 == (k1 & n))
   {
    System.out.printf("Maximum between 2^%d and 2^%d was %f\n", lg2, lg2 + 1, amax);
    amax = 0;
    lg2++;
   }
   k1 = n;
  }
 }
}
