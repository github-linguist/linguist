// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/math_nolibc/__tan.c3
// Permalink: https://github.com/c3lang/c3c/blob/b25c573/lib/std/math/math_nolibc/__tan.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math::nolibc @if(env::NO_LIBC);

/* origin: FreeBSD /usr/src/lib/msun/src/k_tan.c */
/*
 * ====================================================
 * Copyright 2004 Sun Microsystems, Inc.  All Rights Reserved.
 *
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================
 */

const double[*] TAN_T = {
             3.33333333333334091986e-01, /* 3FD55555, 55555563 */
             1.33333333333201242699e-01, /* 3FC11111, 1110FE7A */
             5.39682539762260521377e-02, /* 3FABA1BA, 1BB341FE */
             2.18694882948595424599e-02, /* 3F9664F4, 8406D637 */
             8.86323982359930005737e-03, /* 3F8226E3, E96E8493 */
             3.59207910759131235356e-03, /* 3F6D6D22, C9560328 */
             1.45620945432529025516e-03, /* 3F57DBC8, FEE08315 */
             5.88041240820264096874e-04, /* 3F4344D8, F2F26501 */
             2.46463134818469906812e-04, /* 3F3026F7, 1A8D1068 */
             7.81794442939557092300e-05, /* 3F147E88, A03792A6 */
             7.14072491382608190305e-05, /* 3F12B80F, 32F0A7E9 */
            -1.85586374855275456654e-05, /* BEF375CB, DB605373 */
             2.59073051863633712884e-05, /* 3EFB2A70, 74BF7AD4 */
};

fn double __tan(double x, double y, int odd) @extern("__tan") @weak @nostrip
{
	const double PIO4 = 7.85398163397448278999e-01; /* 3FE921FB, 54442D18 */
	const double PIO4LO = 3.06161699786838301793e-17; /* 3C81A626, 33145C07 */

	uint hx = (uint)(bitcast(x, ulong) >> 32);
	bool big = (hx &0x7fffffff) >= 0x3FE59428; // |x| >= 0.6744
	int sign @noinit;
	if (big)
	{
		sign = hx >> 31;
		if (sign)
		{
			x = -x;
			y = -y;
		}
		x = (PIO4 - x) + (PIO4LO - y);
		y = 0.0;
	}
	double z = x * x;
	double w = z * z;
	/*
	 * Break x^5*(T[1]+x^2*T[2]+...) into
	 * x^5(T[1]+x^4*T[3]+...+x^20*T[11]) +
	 * x^5(x^2*(T[2]+x^4*T[4]+...+x^22*[T12]))
	 */
	double r = TAN_T[1] + w * (TAN_T[3] + w * (TAN_T[5] + w * (TAN_T[7] + w * (TAN_T[9] + w * TAN_T[11]))));
	double v = z * (TAN_T[2] + w * (TAN_T[4] + w * (TAN_T[6] + w * (TAN_T[8] + w * (TAN_T[10] + w * TAN_T[12])))));
	double s = z * x;
	r = y + z * (s * (r + v) + y) + s * TAN_T[0];
	w = x + r;
	if (big)
	{
		s = (double)(1 - 2 * odd);
		v = s - 2.0 * (x + (r - w*w/(w + s)));
		return sign ? -v : v;
	}
	if (!odd) return w;
	// -1.0/(x+r) has up to 2ulp error, so compute it accurately

	// Clear low word
	ulong d = bitcast(w, ulong) & 0xFFFF_FFFF_0000_0000;
	double w0 = bitcast(d, double);

	v = r - (w0 - x);       // w0+v = r+x
	double a = -1.0 / w;
	d = bitcast(a, ulong) & 0xFFFF_FFFF_0000_0000;
	double a0 = bitcast(d, double);
	return a0 + a * (1.0 + a0 * w0 + a0 * v);
}
