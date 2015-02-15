#include <stdio.h>
#include <math.h>

double rms(double *v, int n)
{
  int i;
  double sum = 0.0;
  for(i = 0; i < n; i++)
    sum += v[i] * v[i];
  return sqrt(sum / n);
}

int main(void)
{
  double v[] = {1., 2., 3., 4., 5., 6., 7., 8., 9., 10.};
  printf("%f\n", rms(v, sizeof(v)/sizeof(double)));
  return 0;
}
