// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/math_nolibc/cos.c3
// Permalink: https://github.com/c3lang/c3c/blob/3929e20/lib/std/math/math_nolibc/cos.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math::nolibc @if(env::NO_LIBC);

fn float _cosf(float x) @extern("cosf") @weak @nostrip
{
	uint ix = bitcast(x, uint);
	uint sign = ix >> 31;
	ix &= 0x7fffffff;

	switch
	{
		case (ix <= 0x3f490fda):
			// |x| < 2**-12
			if (ix < 0x39800000)
			{
				/* raise inexact if x != 0 */
				force_eval_add(x, 0x1p120f);
				return 1.0f;
			}
			return __cosdf(x);
		// |x| ~<= 5*pi/4
		case (ix <= 0x407b53d1):
			// |x|  ~> 3*pi/4
			if (ix > 0x4016cbe3) return -__cosdf(sign ? x + S2PI2 : x - S2PI2);
			return sign ? __sindf(x + S1PI2) : __sindf(S1PI2 - x);
		// |x| ~<= 9*pi/4
		case (ix <= 0x40e231d5):
			if (ix > 0x40afeddf) return __cosdf(sign ? x + S4PI2 : x - S4PI2);
			return sign ? __sindf((double)(-x) - S3PI2) : __sindf(x - S3PI2);
	}

	// general argument reduction needed
	double y @noinit;
	switch (__rem_pio2f(x, &y) & 3)
	{
		case 0: return  __cosdf(y);
		case 1: return  __sindf(-y);
		case 2: return  -__cosdf(y);
		default: return __sindf(y);
	}
}

/* origin: FreeBSD /usr/src/lib/msun/src/s_cos.c */
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

fn double _cos(double x) @weak @nostrip
{
	// High word of x.
	uint ix = (uint)(bitcast(x, ulong) >> 32);
	ix &= 0x7fffffff;

	switch
	{
		// |x| ~< pi/4
		case ix <= 0x3fe921fb:
		    if (ix < 0x3e46a09e)
		    {
		        // |x| < 2**-27 * sqrt(2)
			    // raise inexact if x!=0
			    force_eval_add(x, 0x1p120f);
			    return 1.0;
		    }
		    return __cos(x, 0);
		case ix >= 0x7ff00000:
			// cos(Inf or NaN) is NaN
			return x - x;
		default:
			// argument reduction
			double[2] y @noinit;
			switch (__rem_pio2(x, &y) & 3)
			{
				case 0: return __cos(y[0], y[1]);
				case 1: return -__sin(y[0], y[1], 1);
				case 2: return -__cos(y[0], y[1]);
				default: return __sin(y[0], y[1], 1);
			}
	}
}
