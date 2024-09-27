// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/math_nolibc/sincos.c3
// Permalink: https://github.com/c3lang/c3c/blob/4c1edfb/lib/std/math/math_nolibc/sincos.c3
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

fn void sincosf(float x, float *sin, float *cos) @extern("sincosf") @weak @nostrip
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
				// raise inexact if x!=0 and underflow if subnormal
				float z;
				@volatile_store(z, ix < 0x00100000 ? @volatile_load(x) / 0x1p120f : @volatile_load(x) + 0x1p120f);
				*sin = x;
				*cos = 1.0f;
				return;
			}
			*sin = __sindf(x);
			*cos = __cosdf(x);
			return;
		// |x| ~<= 5*pi/4
		case ix <= 0x407b53d1:
			// |x| ~<= 3pi/4
			if (ix <= 0x4016cbe3)
			{
				if (sign)
				{
					*sin = -__cosdf(x + S1PI2);
					*cos = __sindf(x + S1PI2);
					return;
				}
				*sin = __cosdf(S1PI2 - x);
				*cos = __sindf(S1PI2 - x);
				return;
			}
			/* -sin(x+c) is not correct if x+c could be 0: -0 vs +0 */
			*sin = -__sindf(sign ? x + S2PI2 : x - S2PI2);
			*cos = -__cosdf(sign ? x + S2PI2 : x - S2PI2);
			return;
		case ix <= 0x40e231d5:
			// |x| ~<= 9*pi/4
			if (ix <= 0x40afeddf)
			{
				// |x| ~<= 7*pi/4
        		if (sign)
        		{
        		    *sin = __cosdf(x + S3PI2);
        			*cos = -__sindf(x + S3PI2);
        			return;
        		}
        		*sin = -__cosdf(x - S3PI2);
        		*cos = __sindf(x - S3PI2);
        		return;
        	}
        	*sin = __sindf(sign ? x + S4PI2 : x - S4PI2);
        	*cos = __cosdf(sign ? x + S4PI2 : x - S4PI2);
        case ix >= 0x7f800000:
            // sin(Inf or NaN) is NaN
			*sin = *cos = x - x;
		default:
			// general argument reduction needed
			double y @noinit;
			uint n = __rem_pio2f(x, &y);
			float s = __sindf(y);
			float c = __cosdf(y);
			switch (n & 3)
			{
				case 0:
					*sin = s;
					*cos = c;
				case 1:
					*sin = c;
					*cos = -s;
				case 2:
					*sin = -s;
					*cos = -c;
				case 3:
				default:
					*sin = -c;
					*cos = s;
			}
	}

}

fn void sincos(double x, double *sin, double *cos) @extern("sincos") @weak @nostrip
{
	// High word of x.
	uint ix = (uint)(bitcast(x, ulong) >> 32);
	ix &= 0x7fffffff;

	// |x| ~< pi/4
	switch
	{
		// |x| ~< pi/4
		case ix <= 0x3fe921fb:
			if (ix < 0x3e46a09e)
			{
				// raise inexact if x!=0 and underflow if subnormal
				force_eval_add(ix < 0x00100000 ? x/0x1p120f : x+0x1p120f, 0);
				*sin = x;
				*cos = 1.0;
				return;
			}
			// if |x| < 2**-27 * sqrt(2)
			*sin = __sin(x, 0.0, 0);
			*cos = __cos(x, 0.0);
		// sincos(Inf or NaN) is NaN
		case ix >= 0x7ff00000:
    		*sin = *cos = x - x;
    	default:
			// argument reduction needed
			double[2] y;
			int n = __rem_pio2(x, &y);
			double s = __sin(y[0], y[1], 1);
			double c = __cos(y[0], y[1]);
			switch (n & 3)
			{
				case 0:
					*sin = s;
					*cos = c;
				case 1:
					*sin = c;
					*cos = -s;
				case 2:
					*sin = -s;
					*cos = -c;
				case 3:
				default:
					*sin = -c;
					*cos = s;
			}
	}
}
