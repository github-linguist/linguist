// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/math_nolibc/__tandf.c3
// Permalink: https://github.com/c3lang/c3c/blob/4c1edfb/lib/std/math/math_nolibc/__tandf.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math::nolibc @if(env::NO_LIBC);

/* origin: FreeBSD /usr/src/lib/msun/src/k_tanf.c */
/*
 * Conversion to float by Ian Lance Taylor, Cygnus Support, ian@cygnus.com.
 * Optimized by Bruce D. Evans.
 */
/*
 * ====================================================
 * Copyright 2004 Sun Microsystems, Inc.  All Rights Reserved.
 *
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================
 */

// |tan(x)/x - t(x)| < 2**-25.5 (~[-2e-08, 2e-08]).
const double[*] TANDF = {
	0x15554d3418c99f.0p-54, /* 0.333331395030791399758 */
	0x1112fd38999f72.0p-55, /* 0.133392002712976742718 */
	0x1b54c91d865afe.0p-57, /* 0.0533812378445670393523 */
	0x191df3908c33ce.0p-58, /* 0.0245283181166547278873 */
	0x185dadfcecf44e.0p-61, /* 0.00297435743359967304927 */
	0x1362b9bf971bcd.0p-59, /* 0.00946564784943673166728 */
};

fn float __tandf(double x, int odd) @extern("__tandf") @weak @nostrip
{
	double z = x * x;
	/*
	 * Split up the polynomial into small independent terms to give
	 * opportunities for parallel evaluation.  The chosen splitting is
	 * micro-optimized for Athlons (XP, X64).  It costs 2 multiplications
	 * relative to Horner's method on sequential machines.
	 *
	 * We add the small terms from lowest degree up for efficiency on
	 * non-sequential machines (the lowest degree terms tend to be ready
	 * earlier).  Apart from this, we don't care about order of
	 * operations, and don't need to to care since we have precision to
	 * spare.  However, the chosen splitting is good for accuracy too,
	 * and would give results as accurate as Horner's method if the
	 * small terms were added from highest degree down.
	 */
	double r = TANDF[4] + z * TANDF[5];
	double t = TANDF[2] + z * TANDF[3];
	double w = z * z;
	double s = z * x;
	double u = TANDF[0] + z * TANDF[1];
	r = (x + s * u) + (s * w) * (t + w * r);
	return (float)(odd ? -1.0 / r : r);
}
