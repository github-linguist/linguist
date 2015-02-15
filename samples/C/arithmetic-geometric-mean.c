#include<math.h>
#include<stdio.h>
#include<stdlib.h>

double agm( double a, double g ) {
   /* arithmetic-geometric mean */
   double iota = 1.0E-16;
   double a1, g1;

   if( a*g < 0.0 ) {
      printf( "arithmetic-geometric mean undefined when x*y<0\n" );
      exit(1);
   }

   while( fabs(a-g)>iota ) {
      a1 = (a + g) / 2.0;
      g1 = sqrt(a * g);

      a = a1;
      g = g1;
   }

   return a;
}

int main( void ) {
   double x, y;
   printf( "Enter two numbers: " );
   scanf( "%lf%lf", &x, &y );
   printf( "The arithmetic-geometric mean is %lf\n", agm(x, y) );
   return 0;
}
