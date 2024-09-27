// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/math_nolibc/trunc.c3
// Permalink: https://github.com/c3lang/c3c/blob/4c1edfb/lib/std/math/math_nolibc/trunc.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math::nolibc @if(env::NO_LIBC);

fn double _trunc(double x) @weak @extern("trunc") @nostrip
{
	ulong i = bitcast(x, ulong);
	int e = (int)((i >> 52) & 0x7ff) - 0x3ff + 12;
	if (e >= 52 + 12) return x;
	if (e < 12) e = 1;
	ulong m = ((ulong)-1) >> e;
	if (i & m == 0) return x;
	force_eval_add(x, 0x1p120f);
	i &= ~m;
	return bitcast(i, double);
}

fn float _truncf(float x) @weak @extern("truncf") @nostrip
{
	uint i = bitcast(x, uint);
	int e = (int)((i >> 23) & 0xff) - 0x7f + 9;
	if (e >= 23 + 9) return x;
	if (e < 9) e = 1;
	uint m = ((uint)-1) >> e;
	if (i & m == 0) return x;
	force_eval_add(x, 0x1p120f);
	i &= ~m;
	return bitcast(i, float);
}

