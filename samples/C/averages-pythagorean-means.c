#include <stdio.h>
#include <stdlib.h> // atoi()
#include <math.h> // pow()

int main(int argc, char* argv[])
{
  int i, count=0;
  double f, sum=0.0, prod=1.0, resum=0.0;

  for (i=1; i<argc; ++i) {
    f = atof(argv[i]);
    count++;
    sum += f;
    prod *= f;
    resum += (1.0/f);
  }
  //printf(" c:%d\n s:%f\n p:%f\n r:%f\n",count,sum,prod,resum);
  printf("Arithmetic mean = %f\n",sum/count);
  printf("Geometric mean = %f\n",pow(prod,(1.0/count)));
  printf("Harmonic mean = %f\n",count/resum);

  return 0;
}
