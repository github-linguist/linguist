// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/math_nolibc/exp2.c3
// Permalink: https://github.com/c3lang/c3c/blob/4c1edfb/lib/std/math/math_nolibc/exp2.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math::nolibc @if(env::NO_LIBC);

macro uint _top12f(float x) @private => bitcast(x, uint) >> 20;


fn float _exp2f(float x) @extern("exp2f") @weak @nostrip
{
	double xd = x;
	uint abstop = _top12f(x) & 0x7ff;
	if (abstop >= _top12f(128.0f)) /* @unlikely */
	{
		switch
		{
			// |x| >= 128 or x is nan.
			case bitcast(x, uint) == bitcast(-float.inf, uint):
				return 0;
			case abstop >= _top12f(float.inf):
				return x + x;
			case x > 0:
				return __math_oflowf(0);
			case x <= -150.0f:
				return __math_uflowf(0);
		}
	}
	const SHIFT = __EXP2F_DATA.shift_scaled;
	const uint N = 1U << EXP2F_TABLE_BITS;
	// x = k/N + r with r in [-1/(2N), 1/(2N)] and int k.
	double kd = xd + SHIFT;
	ulong ki = bitcast(kd, ulong);
	kd -= SHIFT; /* k/N for int k.  */
	double r = xd - kd;

	// exp2(x) = 2^(k/N) * 2^r ~= s * (C0*r^3 + C1*r^2 + C2*r + 1)
	ulong t = __EXP2F_DATA.tab[ki % N];
	t += ki << (52 - EXP2F_TABLE_BITS);
	double s = bitcast(t, double);
	double z = __EXP2F_DATA.poly[0] * r + __EXP2F_DATA.poly[1];
	double r2 = r * r;
	double y = __EXP2F_DATA.poly[2] * r + 1;
	y = z * r2 + y;
	y = y * s;
	return (float)y;
}

fn double _exp2_specialcase(double tmp, ulong sbits, ulong ki) @private
{
	if (ki & 0x80000000 == 0)
	{
		// k > 0, the exponent of scale might have overflowed by 1.
		sbits -= 1u64 << 52;
		double scale = bitcast(sbits, double);
		double y = 2 * (scale + scale * tmp);
		return y;
	}
	// k < 0, need special care in the subnormal range.
	sbits += 1022u64 << 52;
	double scale = bitcast(sbits, double);
	double y = scale + scale * tmp;
	if (y >= 1.0)
	{
		return 0x1p-1022 * y;
	}
	// Round y to the right precision before scaling it into the subnormal
	// range to avoid double rounding that can cause 0.5+E/2 ulp error where
	// E is the worst-case ulp error outside the subnormal range.  So this
	// is only useful if the goal is better than 1 ulp worst-case error.
	double lo = scale - y + scale * tmp;
	double hi = 1.0 + y;
	lo = 1.0 - hi + y + lo;
	y = hi + lo - 1.0;
	/* Avoid -0.0 with downward rounding.  */
	if (WANT_ROUNDING && y == 0.0) y = 0.0;
	/* The underflow exception needs to be signaled explicitly.  */
	return __math_xflow(0, 0x1p-1022);
}


macro uint _top12d(double x) @private
{
	return (uint)(bitcast(x, ulong) >> 52);
}

fn double _exp2(double x) @extern("exp2") @weak @nostrip
{
	uint abstop = _top12d(x) & 0x7ff;
	ulong u = bitcast(x, ulong);
	if (abstop - _top12d(0x1p-54) >= _top12d(512.0) - _top12d(0x1p-54)) /* @unlikely */
	{
		if (abstop - _top12d(0x1p-54) >= 0x80000000)
		{
			// Avoid spurious underflow for tiny x.  */
			// Note: 0 is common input.  */
			return WANT_ROUNDING ? 1.0 + x : 1.0;
		}
		if (abstop >= _top12d(1024.0))
		{
			switch
			{
				case u == bitcast(-double.inf, ulong):
					return 0.0;
				case abstop >= _top12d(double.inf):
					return 1.0 + x;
				case !(u >> 63):
					return __math_oflow(0);
				case u >= bitcast(-1075.0, ulong):
					return __math_uflow(0);
			}
		}
		// Large x is special cased below.
		if (2 * u > 2 * bitcast(928.0, ulong)) abstop = 0;
	}
	const SHIFT = __EXP2_DATA.exp2_shift;
	// exp2(x) = 2^(k/N) * 2^r, with 2^r in [2^(-1/2N),2^(1/2N)].  */
	// x = k/N + r, with int k and r in [-1/2N, 1/2N].  */
	double kd = x + SHIFT;
	ulong ki = bitcast(kd, ulong); /* k.  */
	kd -= SHIFT; /* k/N for int k.  */
	double r = x - kd;
	/* 2^(k/N) ~= scale * (1 + tail).  */
	ulong idx = 2 * (ki % EXP_DATA_WIDTH);
	ulong top = ki << (52 - EXP_TABLE_BITS);
	double tail = __EXP2_DATA.tab[idx];
	/* This is only a valid scale when -1023*N < k < 1024*N.  */
	ulong sbits = __EXP2_DATA.tab[idx + 1] + top;
	/* exp2(x) = 2^(k/N) * 2^r ~= scale + scale * (tail + 2^r - 1).  */
	/* Evaluation is optimized assuming superscalar pipelined execution.  */
	double r2 = r * r;
	const C1 = __EXP2_DATA.exp2_poly[0];
    const C2 = __EXP2_DATA.exp2_poly[1];
    const C3 = __EXP2_DATA.exp2_poly[2];
    const C4 = __EXP2_DATA.exp2_poly[3];
    const C5 = __EXP2_DATA.exp2_poly[4];
	/* Without fma the worst case error is 0.5/N ulp larger.  */
	/* Worst case error is less than 0.5+0.86/N+(abs poly error * 2^53) ulp.  */
	double tmp = tail + r * C1 + r2 * (C2 + r * C3) + r2 * r2 * (C4 + r * C5);
	if (abstop == 0 /* @unlikely */) return _exp2_specialcase(tmp, sbits, ki);
	double scale = bitcast(sbits, double);
	/* Note: tmp == 0 or |tmp| > 2^-65 and scale > 2^-928, so there
	   is no spurious underflow here even without fma.  */
	return scale + scale * tmp;
}
