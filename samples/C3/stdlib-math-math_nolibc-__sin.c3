// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/math_nolibc/__sin.c3
// Permalink: https://github.com/c3lang/c3c/blob/4c1edfb/lib/std/math/math_nolibc/__sin.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math::nolibc @if(env::NO_LIBC);

/* origin: FreeBSD /usr/src/lib/msun/src/k_sin.c */
/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunSoft, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================
 */
fn double __sin(double x, double y, int iy) @extern("__sin") @weak @nostrip
{

	const S1 = -1.66666666666666324348e-01; /* 0xBFC55555, 0x55555549 */
	const S2 = 8.33333333332248946124e-03; /* 0x3F811111, 0x1110F8A6 */
	const S3 = -1.98412698298579493134e-04; /* 0xBF2A01A0, 0x19C161D5 */
	const S4 = 2.75573137070700676789e-06; /* 0x3EC71DE3, 0x57B1FE7D */
	const S5 = -2.50507602534068634195e-08; /* 0xBE5AE5E6, 0x8A2B9CEB */
	const S6 = 1.58969099521155010221e-10; /* 0x3DE5D93A, 0x5ACFD57C */

	double z = x * x;
	double w = z * z;
	double r = S2 + z * (S3 + z * S4) + z * w * (S5 + z * S6);
	double v = z * x;
	return iy == 0
		? x + v * (S1 + z * r)
		: x - ((z * (0.5 * y - v * r) - y) - v * S1);
}
