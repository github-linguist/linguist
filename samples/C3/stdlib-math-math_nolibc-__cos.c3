// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/math_nolibc/__cos.c3
// Permalink: https://github.com/c3lang/c3c/blob/4c1edfb/lib/std/math/math_nolibc/__cos.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math::nolibc @if(env::NO_LIBC);

/* origin: FreeBSD /usr/src/lib/msun/src/k_cos.c */
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

fn double __cos(double x, double y) @extern("__cos") @weak @nostrip
{
	const C1 =  4.16666666666666019037e-02; /* 0x3FA55555, 0x5555554C */
	const C2 = -1.38888888888741095749e-03; /* 0xBF56C16C, 0x16C15177 */
	const C3 =  2.48015872894767294178e-05; /* 0x3EFA01A0, 0x19CB1590 */
	const C4 = -2.75573143513906633035e-07; /* 0xBE927E4F, 0x809C52AD */
	const C5 =  2.08757232129817482790e-09; /* 0x3E21EE9E, 0xBDB4B1C4 */
	const C6 = -1.13596475577881948265e-11; /* 0xBDA8FAE9, 0xBE8838D4 */

	double z = x * x;
	double w = z * z;
	double r = z * (C1 + z * (C2 + z * C3)) + w * w * (C4 + z * (C5 + z * C6));
	double hz = 0.5 * z;
	w = 1.0 - hz;
	return w + (((1.0 - w) - hz) + (z * r - x * y));
}

