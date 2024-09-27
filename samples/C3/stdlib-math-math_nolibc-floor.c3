// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/math_nolibc/floor.c3
// Permalink: https://github.com/c3lang/c3c/blob/4c1edfb/lib/std/math/math_nolibc/floor.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math::nolibc @if(env::NO_LIBC);

fn double _floor(double x) @weak @extern("floor") @nostrip
{
	ulong ui = bitcast(x, ulong);
	int e = (int)((ui >> 52) & 0x7ff);
	if (e >= 0x3ff + 52 || x == 0) return x;
	// y = int(x) - x, where int(x) is an integer neighbor of x
	double y = ui >> 63 ? (x - TOINT) + TOINT - x : (x + TOINT) - TOINT - x;
	// special case because of non-nearest rounding modes
	if (e <= 0x3ff - 1)
	{
		force_eval_add(y, 0);
		return ui >> 63 ? -1 : 0;
	}
	return y > 0 ? x + y - 1 : x + y;
}


fn float _floorf(float x) @weak @extern("floorf") @nostrip
{
	uint u = bitcast(x, uint);
	int e = (int)((u >> 23) & 0xff) - 0x7f;
	switch
	{
		case e >= 23:
			return x;
		case e >= 0:
			uint m = 0x007fffff >> e;
			if (u & m == 0) return x;
			force_eval_add(x, 0x1p120f);
			if (u >> 31) u += m;
			u &= ~m;
		case u >> 31 == 0:
			force_eval_add(x, 0x1p120f);
			u = 0;
		case (u << 1 != 0):
			return -1;
	}
	return bitcast(u, float);
}

