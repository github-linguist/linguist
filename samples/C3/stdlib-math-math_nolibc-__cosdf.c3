// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/math_nolibc/__cosdf.c3
// Permalink: https://github.com/c3lang/c3c/blob/4c1edfb/lib/std/math/math_nolibc/__cosdf.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math::nolibc @if(env::NO_LIBC);

/* origin: FreeBSD /usr/src/lib/msun/src/k_cosf.c */
/*
 * Conversion to float by Ian Lance Taylor, Cygnus Support, ian@cygnus.com.
 * Debugged and optimized by Bruce D. Evans.
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
/* |cos(x) - c(x)| < 2**-34.1 (~[-5.37e-11, 5.295e-11]). */

const double C0 @private = -0x1ffffffd0c5e81.0p-54; /* -0.499999997251031003120 */
const double C1 @private =  0x155553e1053a42.0p-57; /*  0.0416666233237390631894 */
const double C2 @private = -0x16c087e80f1e27.0p-62; /* -0.00138867637746099294692 */
const double C3 @private =  0x199342e0ee5069.0p-68; /*  0.0000243904487962774090654 */

fn float __cosdf(double x) @extern("__cosdf") @weak @nostrip
{
	/* Try to optimize for parallel evaluation as in __tandf.c. */
	double z = x * x;
	double w = z * z;
	double r = C2 + z * C3;
	return (float)(((1.0f + z * C0) + w * C1) + (w * z) * r);
}
