#include <stdio.h>
#include <assert.h>

int ipow(int base, int exp)
{
   int pow = base;
   int v = 1;
   if (exp < 0) {
      assert (base != 0);  /* divide by zero */
      return (base*base != 1)? 0: (exp&1)? base : 1;
   }

   while(exp > 0 )
   {
      if (exp & 1) v *= pow;
      pow *= pow;
      exp >>= 1;
    }
   return v;
}

double dpow(double base, int exp)
{
   double v=1.0;
   double pow = (exp <0)? 1.0/base : base;
   if (exp < 0) exp = - exp;

   while(exp > 0 )
   {
      if (exp & 1) v *= pow;
      pow *= pow;
      exp >>= 1;
   }
   return v;
}

int main()
{
    printf("2^6 = %d\n", ipow(2,6));
    printf("2^-6 = %d\n", ipow(2,-6));
    printf("2.71^6 = %lf\n", dpow(2.71,6));
    printf("2.71^-6 = %lf\n", dpow(2.71,-6));
}
