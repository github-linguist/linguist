/* Abhishek Ghosh
   27th August, 2012 */

#include<math.h>
#include<stdio.h>

int
main ()
{
  double inputs[11], check = 400, result;
  int i;

  printf ("\nPlease enter 11 numbers :");

  for (i = 0; i < 11; i++)
    {
      scanf ("%lf", &inputs[i]);
    }

  printf ("\n\n\nEvaluating f(x) = |x|^0.5 + 5x^3 for the given inputs :");

  for (i = 10; i >= 0; i--)
    {
      result = sqrt (fabs (inputs[i])) + 5 * pow (inputs[i], 3);

      printf ("\nf(%lf) = ");

      if (result > check)
        {
          printf ("Overflow!");
        }

      else
        {
          printf ("%lf", result);
        }
    }

  return 0;
}
