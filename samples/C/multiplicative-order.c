ulong mpow(ulong a, ulong p, ulong m)
{
	ulong r = 1;
	while (p) {
		if ((1 & p)) r = r * a % m;
		a = a * a % m;
		p >>= 1;
	}
	return r;
}

ulong ipow(ulong a, ulong p) {
	ulong r = 1;
	while (p) {
		if ((1 & p)) r = r * a;
		a *= a;
		p >>= 1;
	}
	return r;
}

ulong gcd(ulong m, ulong n)
{
	ulong t;
	while (m) { t = m; m = n % m; n = t; }
	return n;
}

ulong lcm(ulong m, ulong n)
{
	ulong g = gcd(m, n);
	return m / g * n;
}

ulong multi_order_p(ulong a, ulong p, ulong e)
{
	ulong fac[10000];
	ulong m = ipow(p, e);
	ulong t = m / p * (p - 1);
	int i, len = get_factors(t, fac);
	for (i = 0; i < len; i++)
		if (mpow(a, fac[i], m) == 1)
			return fac[i];
	return 0;
}

ulong multi_order(ulong a, ulong m)
{
	prime_factor pf[100];
	int i, len = get_prime_factors(m, pf);
	ulong res = 1;
	for (i = 0; i < len; i++)
		res = lcm(res, multi_order_p(a, pf[i].p, pf[i].e));
	return res;
}

int main()
{
	sieve();
	printf("%lu\n", multi_order(37, 1000));
	printf("%lu\n", multi_order(54, 100001));
	return 0;
}
