// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/math_nolibc/__sindf.c3
// Permalink: https://github.com/c3lang/c3c/blob/4c1edfb/lib/std/math/math_nolibc/__sindf.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math::nolibc @if(env::NO_LIBC);

/* origin: FreeBSD /usr/src/lib/msun/src/k_sinf.c */
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
// |sin(x)/x - s(x)| < 2**-37.5 (~[-4.89e-12, 4.824e-12]).
fn float __sindf(double x) @extern("__sindf") @weak @nostrip
{
	const S1F = -0x15555554cbac77.0p-55; /* -0.166666666416265235595 */
	const S2F =  0x111110896efbb2.0p-59; /*  0.0083333293858894631756 */
	const S3F = -0x1a00f9e2cae774.0p-65; /* -0.000198393348360966317347 */
	const S4F =  0x16cd878c3b46a7.0p-71; /*  0.0000027183114939898219064 */
	double z = x * x;
	double w = z * z;
	double r = S3F + z * S4F;
	double s = z * x;
	return (float)((x + s * (S1F + z * S2F)) +  s * w * r);
}

