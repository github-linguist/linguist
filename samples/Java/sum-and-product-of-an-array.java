public class SumProd
{
 public static void main(final String[] args)
 {
  int sum = 0;
  int prod = 1;
  int[] arg = {1,2,3,4,5};
  for (int i : arg)
  {
   sum += i;
   prod *= i;
  }
 }
}
