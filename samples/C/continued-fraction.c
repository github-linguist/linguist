/* calculate approximations for continued fractions */
#include <stdio.h>

/* kind of function that returns a series of coefficients */
typedef double (*coeff_func)(unsigned n);

/* calculates the specified number of expansions of the continued fraction
 * described by the coefficient series f_a and f_b */
double calc(coeff_func f_a, coeff_func f_b, unsigned expansions)
{
	double a, b, r;
	a = b = r = 0.0;

	unsigned i;
	for (i = expansions; i > 0; i--) {
		a = f_a(i);
		b = f_b(i);
		r = b / (a + r);
	}
	a = f_a(0);

	return a + r;
}

/* series for sqrt(2) */
double sqrt2_a(unsigned n)
{
	return n ? 2.0 : 1.0;
}

double sqrt2_b(unsigned n)
{
	return 1.0;
}

/* series for the napier constant */
double napier_a(unsigned n)
{
	return n ? n : 2.0;
}

double napier_b(unsigned n)
{
	return n > 1.0 ? n - 1.0 : 1.0;
}

/* series for pi */
double pi_a(unsigned n)
{
	return n ? 6.0 : 3.0;
}

double pi_b(unsigned n)
{
	double c = 2.0 * n - 1.0;

	return c * c;
}

int main(void)
{
	double sqrt2, napier, pi;

	sqrt2  = calc(sqrt2_a,  sqrt2_b,  1000);
	napier = calc(napier_a, napier_b, 1000);
	pi     = calc(pi_a,     pi_b,     1000);

	printf("%12.10g\n%12.10g\n%12.10g\n", sqrt2, napier, pi);

	return 0;
}
