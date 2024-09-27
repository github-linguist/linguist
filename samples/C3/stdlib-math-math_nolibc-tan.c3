// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/math_nolibc/tan.c3
// Permalink: https://github.com/c3lang/c3c/blob/4c1edfb/lib/std/math/math_nolibc/tan.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math::nolibc @if(env::NO_LIBC);

/* origin: FreeBSD /usr/src/lib/msun/src/s_tan.c */
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

fn double tan(double x) @extern("tan") @weak @nostrip
{
	uint ix = (uint)(bitcast(x, ulong) >> 32);
	ix &= 0x7fffffff;

	// |x| ~< pi/4
	switch
	{
		case ix <= 0x3fe921fb:
			if (ix < 0x3e400000)
			{
				// |x| < 2**-27 */
				/* raise inexact if x!=0 and underflow if subnormal */
				force_eval_add(ix < 0x00100000 ? x / 0x1p120f : x + 0x1p120f, 0);
				return x;
			}
			return __tan(x, 0.0, 0);
		// tan(Inf or NaN) is NaN
    	case ix >= 0x7ff00000:
    		return x - x;
		default:
			// argument reduction
			double[2] y;
			uint n = __rem_pio2(x, &y);
            return __tan(y[0], y[1], n & 1);

	}
}

/* origin: FreeBSD /usr/src/lib/msun/src/s_tanf.c */
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

fn float tanf(float x) @extern("tanf") @weak @nostrip
{
	uint ix = bitcast(x, uint);
	uint sign = ix >> 31;
	ix &= 0x7fffffff;

	switch
	{
		// |x| ~<= pi/4
		case ix <= 0x3f490fda:
			// |x| < 2**-12
			if (ix < 0x39800000)
			{
				/* raise inexact if x!=0 and underflow if subnormal */
				force_eval_add(ix < 0x00800000 ? x / 0x1p120f : x + 0x1p120f, 0);
				return x;
			}
			return __tandf(x, 0);
		// |x| ~<= 5*pi/4
		case ix <= 0x407b53d1:
			// |x| ~<= 3pi/4
			return ix <= 0x4016cbe3
				? __tandf(sign ? x + S1PI2 : x - S1PI2, 1)
				: __tandf(sign ? x + S2PI2 : x - S2PI2, 0);
		// |x| ~<= 9*pi/4
		case ix <= 0x40e231d5:
			// |x| ~<= 7*pi/4 */
			return ix <= 0x40afeddf
				? __tandf(sign ? x + S3PI2 : x - S3PI2, 1)
				: __tandf(sign ? x + S4PI2 : x - S4PI2, 0);
		// tan(Inf or NaN) is NaN
		case ix >= 0x7f800000:
			return x - x;
		default:
			// argument reduction
			double y;
			uint n = __rem_pio2f(x, &y);
			return __tandf(y, n & 1);
	}
}

