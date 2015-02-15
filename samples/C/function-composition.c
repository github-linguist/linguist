#include <stdlib.h>

/* generic interface for functors from double to double */
typedef struct double_to_double {
  double (*fn)(struct double_to_double *, double);
} double_to_double;

#define CALL(f, x) f->fn(f, x)


/* functor returned by compose */
typedef struct compose_functor {
  double (*fn)(struct compose_functor *, double);
  double_to_double *f;
  double_to_double *g;
} compose_functor;
/* function to be used in "fn" in preceding functor */
double compose_call(compose_functor *this, double x) {
  return CALL(this->f, CALL(this->g, x));
}
/* returns functor that is the composition of functors
   f & g. caller is responsible for deallocating memory */
double_to_double *compose(double_to_double *f,
                          double_to_double *g) {
  compose_functor *result = malloc(sizeof(compose_functor));
  result->fn = &compose_call;
  result->f = f;
  result->g = g;
  return (double_to_double *)result;
}



#include <math.h>

/* we can make functors for sin and asin by using
   the following as "fn" in a functor */
double sin_call(double_to_double *this, double x) {
  return sin(x);
}
double asin_call(double_to_double *this, double x) {
  return asin(x);
}



#include <stdio.h>

int main() {
  double_to_double *my_sin = malloc(sizeof(double_to_double));
  my_sin->fn = &sin_call;
  double_to_double *my_asin = malloc(sizeof(double_to_double));
  my_asin->fn = &asin_call;

  double_to_double *sin_asin = compose(my_sin, my_asin);

  printf("%f\n", CALL(sin_asin, 0.5)); /* prints "0.500000" */

  free(sin_asin);
  free(my_sin);
  free(my_asin);

  return 0;
}
