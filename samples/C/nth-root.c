#include <stdio.h>
#include <float.h>

inline double abs_(double x) { return x >= 0 ? x : -x; }
double pow_(double x, int e)
{
	double ret = 1;
	for (ret = 1; e; x *= x, e >>= 1)
		if ((e & 1)) ret *= x;
	return ret;
}

double root(double a, int n)
{
	double d, x = 1;
	if (!a) return 0;
	if (n < 1 || (a < 0 && !(n&1))) return 0./0.; /* NaN */

	do {	d = (a / pow_(x, n - 1) - x) / n;
		x+= d;
	} while (abs_(d) >= abs_(x) * (DBL_EPSILON * 10));

	return x;
}

int main()
{
	double x = pow_(-3.14159, 15);
	printf("root(%g, 15) = %g\n", x, root(x, 15));
	return 0;
}
