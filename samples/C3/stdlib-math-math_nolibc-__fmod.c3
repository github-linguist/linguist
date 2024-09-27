// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/math_nolibc/__fmod.c3
// Permalink: https://github.com/c3lang/c3c/blob/bc267e2/lib/std/math/math_nolibc/__fmod.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math::nolibc @if(env::NO_LIBC);

union DoubleInternal
{
	double f;
	ulong i;
}

// Based on the musl implementation
fn double fmod(double x, double y) @extern("fmod") @weak @nostrip
{
	DoubleInternal ux = { .f = x };
	DoubleInternal uy = { .f = y };
	int ex = (int)((ux.i >> 52) & 0x7ff);
	int ey = (int)((uy.i >> 52) & 0x7ff);
	int sx = (int)(ux.i >> 63);
	ulong uxi = ux.i;
	if (uy.i << 1 == 0 || math::is_nan(y) || ex == 0x7ff) return (x * y)/(x * y);
	if (uxi << 1 <= uy.i << 1)
	{
		if (uxi << 1 == uy.i << 1) return 0 * x;
		return x;
	}

	if (!ex)
	{
		for (ulong i = uxi << 12; i >> 63 == 0; ex--, i <<= 1);
		uxi <<= -ex + 1;
	}
	else
	{
		uxi &= -1UL >> 12;
		uxi |= 1UL << 52;
	}
	if (!ey)
	{
		for (ulong i = uy.i << 12; i >> 63 == 0; ey--, i <<= 1);
		uy.i <<= -ey + 1;
	}
	else
	{
		uy.i &= -1UL >> 12;
		uy.i |= 1UL << 52;
	}

	/* x mod y */
	for (; ex > ey; ex--)
	{
		ulong i = uxi - uy.i;
		if (i >> 63 == 0)
		{
			if (i == 0) return 0 * x;
			uxi = i;
		}
		uxi <<= 1;
	}
	ulong i = uxi - uy.i;
	if (i >> 63 == 0)
	{
		if (i == 0) return 0*x;
		uxi = i;
	}
	for (; uxi>>52 == 0; uxi <<= 1, ex--);

	/* scale result */
	if (ex > 0)
	{
		uxi -= 1UL << 52;
		uxi |= (ulong)ex << 52;
	}
	else
	{
		uxi >>= -ex + 1;
	}
	uxi |= (ulong)sx << 63;
	ux.i = uxi;
	return ux.f;
}