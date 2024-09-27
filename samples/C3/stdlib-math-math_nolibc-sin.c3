// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/math_nolibc/sin.c3
// Permalink: https://github.com/c3lang/c3c/blob/3929e20/lib/std/math/math_nolibc/sin.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math::nolibc @if(env::NO_LIBC);

/* origin: FreeBSD /usr/src/lib/msun/src/s_sinf.c */
/*
 * Conversion to float by Ian Lance Taylor, Cygnus Support, ian@cygnus.com.
 * Optimized by Bruce D. Evans.
 */
/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================
 */

fn float _sinf(float x) @weak @extern("sinf") @nostrip
{
	uint ix = bitcast(x, uint);
	int sign = ix >> 31;
	ix &= 0x7fffffff;
	switch
	{
		case ix <= 0x3f490fda:
			// |x| ~<= pi/4
			if (ix < 0x39800000)
			{
				/* |x| < 2**-12 */
				// raise inexact if x!=0 and underflow if subnormal
				if (ix < 0x00800000)
				{
					@volatile_load(x) / 0x1p120f;
				}
				else
				{
					@volatile_load(x) + 0x1p120f;
				}
				return x;
			}
			return __sindf(x);
		case ix <= 0x407b53d1:
		    // |x| ~<= 5*pi/4
			if (ix <= 0x4016cbe3)
			{
				// |x| ~<= 3pi/4
				if (sign) return -__cosdf(x + S1PI2);
				return __cosdf(x - S1PI2);
			}
			return __sindf(sign ? -(x + S2PI2) : -(x - S2PI2));
		case ix <= 0x40e231d5:
		    // |x| ~<= 9*pi/4
			if (ix <= 0x40afeddf)
			{
				// |x| ~<= 7*pi/4
				return sign ? __cosdf(x + S3PI2) : -__cosdf(x - S3PI2);
			}
			return __sindf(sign ? x + S4PI2 : x - S4PI2);
		case ix >= 0x7f800000:
			// sin(Inf or NaN) is NaN
			return x - x;
	}
	/* general argument reduction needed */
	double y @noinit;
	switch (__rem_pio2f(x, &y) & 3)
	{
		case 0: return  __sindf(y);
		case 1: return  __cosdf(y);
		case 2: return  __sindf(-y);
		default: return -__cosdf(y);
	}
}

/* origin: FreeBSD /usr/src/lib/msun/src/s_sin.c */
/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================
 */

fn double sin(double x) @extern("sin") @weak @nostrip
{
	// High word of x.
	uint ix = (uint)(bitcast(x, ulong) >> 32);
	ix &= 0x7fffffff;
	switch
	{
		// |x| ~< pi/4
		case ix <= 0x3fe921fb:
			// |x| < 2**-26
			if (ix < 0x3e500000)
			{
				// raise inexact if x != 0 and underflow if subnormal
				force_eval_add(x, ix < 0x00100000 ? -0x1p120f : 0x1p120f);
				return x;
			}
			return __sin(x, 0.0, 0);
		// sin(Inf or NaN) is NaN
		case ix >= 0x7ff00000:
			return x - x;
		default:
			// argument reduction needed
			double[2] y;
			switch (__rem_pio2(x, &y) & 3)
			{
				case 0: return __sin(y[0], y[1], 1);
				case 1: return __cos(y[0], y[1]);
				case 2: return -__sin(y[0], y[1], 1);
				default: return -__cos(y[0], y[1]);
			}
	}
}
