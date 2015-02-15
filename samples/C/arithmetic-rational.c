#include <stdio.h>
#include <stdlib.h>
#define FMT "%lld"
typedef long long int fr_int_t;
typedef struct { fr_int_t num, den; } frac;

fr_int_t gcd(fr_int_t m, fr_int_t n)
{
	fr_int_t t;
	while (n) { t = n; n = m % n; m = t; }
	return m;
}

frac frac_new(fr_int_t num, fr_int_t den)
{
	frac a;
	if (!den) {
		printf("divide by zero: "FMT"/"FMT"\n", num, den);
		abort();
	}

	int g = gcd(num, den);

	if (g)	{ num /= g; den /= g; }
	else	{ num = 0; den = 1;   }

	if (den < 0) {
		den = -den;
		num = -num;
	}
	a.num = num; a.den = den;
	return a;
}

#define BINOP(op, n, d) frac frac_##op(frac a, frac b) { return frac_new(n,d); }
BINOP(add, a.num * b.den + b.num * a.den, a.den * b.den);
BINOP(sub, a.num * b.den - b.num + a.den, a.den * b.den);
BINOP(mul, a.num * b.num, a.den * b.den);
BINOP(div, a.num * b.den, a.den * b.num);

int frac_cmp(frac a, frac b) {
	int l = a.num * b.den, r = a.den * b.num;
	return l < r ? -1 : l > r;
}
#define frac_cmp_int(a, b) frac_cmp(a, frac_new(b, 1))
int frtoi(frac a) { return a.den / a.num; }
double frtod(frac a) { return (double)a.den / a.num; }

int main()
{
	int n, k;
	frac sum, kf;

	for (n = 2; n < 1<<19; n++) {
		sum = frac_new(1, n);

		for (k = 2; k * k < n; k++) {
			if (n % k) continue;
			kf = frac_new(1, k);
			sum = frac_add(sum, kf);

			kf = frac_new(1, n / k);
			sum = frac_add(sum, kf);
		}
		if (frac_cmp_int(sum, 1) == 0) printf("%d\n", n);
	}

	return 0;
}
