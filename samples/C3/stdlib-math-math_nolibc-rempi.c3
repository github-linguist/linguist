// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/math_nolibc/rempi.c3
// Permalink: https://github.com/c3lang/c3c/blob/b25c573/lib/std/math/math_nolibc/rempi.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math::nolibc @if(env::NO_LIBC);
import std::math;

/* origin: FreeBSD /usr/src/lib/msun/src/e_rem_pio2f.c */
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
/* __rem_pio2f(x,y)
 *
 * return the remainder of x rem pi/2 in *y
 * use double precision for everything except passing x
 * use __rem_pio2_large() for large x
 */

/*
 * invpio2:  53 bits of 2/pi
 * pio2_1:   first 25 bits of pi/2
 * pio2_1t:  pi/2 - pio2_1
 */
fn int __rem_pio2f(float x, double *y)
{
	const double PIO4 = 0x1.921fb6p-1;
	const double INVPIO2 = 6.36619772367581382433e-01; /* 0x3FE45F30, 0x6DC9C883 */
	const double PIO2_1 = 1.57079631090164184570e+00; /* 0x3FF921FB, 0x50000000 */
	const double PIO2_1T = 1.58932547735281966916e-08; /* 0x3E5110b4, 0x611A6263 */

	uint ux = bitcast(x, uint);
	uint ix = ux & 0x7fffffff;
	/* 25+53 bit pi is good enough for medium size */
	if (ix < 0x4dc90fdb)
	{
		// |x| ~< 2^28*(pi/2), medium size
		// Use a specialized rint() to get f.
		uint f = (uint)(((double)x * INVPIO2 + TOINT15) - TOINT15);
		int n  = (int)f;
		*y = x - f * PIO2_1 - f * PIO2_1T;
		/* Matters with directed rounding. */
		if (*y < -PIO4)  // likely false
		{
			n--;
			f--;
			*y = x - f * PIO2_1 - f * PIO2_1T;
		}
		else if (*y > PIO4) // likely false
		{
			n++;
			f++;
			*y = x - f * PIO2_1 - f * PIO2_1T;
		}
		return n;
	}
	if (ix >= 0x7f800000)
	{
		// x is inf or NaN */
		*y = x - (double)x;
		return 0;
	}
	/* scale x into [2^23, 2^24-1] */
	int sign = ux >> 31;
	int e0 = (ix >> 23) - (0x7f + 23);  /* e0 = ilogb(|x|)-23, positive */
	ux = ix - e0 << 23;
	double tx = bitcast(ux, float);
	double ty @noinit;
	int n = __rem_pio2_large(&tx,&ty, e0, 1, 0);
	if (sign)
	{
		*y = (float)-ty;
		return -n;
	}
	*y = (float)ty;
	return n;
}

/* origin: FreeBSD /usr/src/lib/msun/src/k_rem_pio2.c */
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

const int[*] INIT_JK = {3,4,4,6}; /* initial value for jk */

const int[*] IPIO2 = {
0xA2F983, 0x6E4E44, 0x1529FC, 0x2757D1, 0xF534DD, 0xC0DB62,
0x95993C, 0x439041, 0xFE5163, 0xABDEBB, 0xC561B7, 0x246E3A,
0x424DD2, 0xE00649, 0x2EEA09, 0xD1921C, 0xFE1DEB, 0x1CB129,
0xA73EE8, 0x8235F5, 0x2EBB44, 0x84E99C, 0x7026B4, 0x5F7E41,
0x3991D6, 0x398353, 0x39F49C, 0x845F8B, 0xBDF928, 0x3B1FF8,
0x97FFDE, 0x05980F, 0xEF2F11, 0x8B5A0A, 0x6D1F6D, 0x367ECF,
0x27CB09, 0xB74F46, 0x3F669E, 0x5FEA2D, 0x7527BA, 0xC7EBE5,
0xF17B3D, 0x0739F7, 0x8A5292, 0xEA6BFB, 0x5FB11F, 0x8D5D08,
0x560330, 0x46FC7B, 0x6BABF0, 0xCFBC20, 0x9AF436, 0x1DA9E3,
0x91615E, 0xE61B08, 0x659985, 0x5F14A0, 0x68408D, 0xFFD880,
0x4D7327, 0x310606, 0x1556CA, 0x73A8C9, 0x60E27B, 0xC08C6B, };

const double[*] PIO2 = {
  1.57079625129699707031e+00, /* 0x3FF921FB, 0x40000000 */
  7.54978941586159635335e-08, /* 0x3E74442D, 0x00000000 */
  5.39030252995776476554e-15, /* 0x3CF84698, 0x80000000 */
  3.28200341580791294123e-22, /* 0x3B78CC51, 0x60000000 */
  1.27065575308067607349e-29, /* 0x39F01B83, 0x80000000 */
  1.22933308981111328932e-36, /* 0x387A2520, 0x40000000 */
  2.73370053816464559624e-44, /* 0x36E38222, 0x80000000 */
  2.16741683877804819444e-51, /* 0x3569F31D, 0x00000000 */
};

fn int __rem_pio2_large(double* x, double* y, int e0, int nx, int prec)
{
	double[20] f @noinit;
	double[20] fq @noinit;
	double[20] q @noinit;
	int[20] iq @noinit;
	double fw @noinit;
	/* initialize jk*/
	int jk = INIT_JK[prec];
	int jp = jk;

	/* determine jx,jv,q0, note that 3>q0 */
	int jx = nx - 1;
	int jv = (e0 - 3) / 24;
	if (jv < 0) jv = 0;
	int q0 = e0 - 24 * (jv + 1);

	// set up f[0] to f[jx+jk] where f[jx+jk] = ipio2[jv+jk]
	int j = jv - jx;
	int m = jx + jk;
	for (int i = 0; i <= m; i++, j++)
	{
		f[i] = j < 0 ? 0.0 : (double)IPIO2[j];
	}

	// compute q[0],q[1],...q[jk]
	for (int i = 0; i <= jk; i++)
	{
		for (j = 0, fw = 0.0; j <= jx; j++) fw += x[j] * f[jx + i - j];
		q[i] = fw;
	}

	int jz = jk;
	double z @noinit;
	int ih = 0;
	int n @noinit;
	while (1)
	{
		/* distill q[] into iq[] reversingly */
		z = q[jz];
		j = jz;
		for (int i = 0; j > 0; i++, j--)
		{
			fw = (double)(int)(0x1p-24 * z);
			iq[i] = (int)(z - 0x1p24 * fw);
			z = q[j - 1] + fw;
		}
		// compute n
		z  = nolibc::_scalbn(z, q0);       /* actual value of z */
		z -= 8.0 * nolibc::_floor(z * 0.125); /* trim off integer >= 8 */
		n = (int)z;
		z -= (double)n;
		switch
		{
			case q0 > 0:
				// need iq[jz-1] to determine n
				int i = iq[jz - 1] >> (24 - q0);
				n += i;
				iq[jz - 1] -= i << (24 - q0);
				ih = iq[jz - 1] >> (23 - q0);
			case q0 == 0:
			    ih = iq[jz - 1] >> 23;
			case z >= 0.5:
			    ih = 2;
		}
		if (ih > 0)
		{
			/* q > 0.5 */
			n += 1;
			int carry = 0;
			for (int i = 0; i < jz; i++)
			{
			    /* compute 1-q */
				j = iq[i];
				if (carry == 0)
				{
					if (j != 0)
					{
						carry = 1;
						iq[i] = 0x1000000 - j;
					}
				}
				else
				{
					iq[i] = 0xffffff - j;
				}
			}
			if (q0 > 0)
			{
				/* rare case: chance is 1 in 12 */
				switch (q0)
				{
					case 1: iq[jz - 1] &= 0x7fffff;
					case 2: iq[jz - 1] &= 0x3fffff;
				}
			}
			if (ih == 2)
			{
				z = 1.0 - z;
				if (carry != 0) z -= nolibc::_scalbn(1.0, q0);
			}
		}

		// check if recomputation is needed
		if (z == 0.0)
		{
			j = 0;
			for (int i = jz - 1; i >= jk; i--) j |= iq[i];
			if (j == 0)
			{
				/* need recomputation */
				int k = 1;
				for (; iq[jk - k] == 0; k++);  /* k = no. of terms needed */

				for (int i = jz + 1; i <= jz + k; i++)
				{
					/* add q[jz+1] to q[jz+k] */
					f[jx + i] = (double)IPIO2[jv + i];
					for (j = 0, fw = 0.0; j <= jx; j++)
					fw += x[j] * f[jx + i - j];
					q[i] = fw;
				}
				jz += k;
				continue;
			}
		}
		break;
	}

	/* chop off zero terms */
	if (z == 0.0)
	{
		jz -= 1;
		q0 -= 24;
		while (iq[jz] == 0)
		{
			jz--;
			q0 -= 24;
		}
	}
	else
	{
		/* break z into 24-bit if necessary */
		z = _scalbn(z,-q0);
		if (z >= 0x1p24)
		{
			fw = (double)(int)(0x1p-24 * z);
			iq[jz] = (int)(z - 0x1p24 * fw);
			jz += 1;
			q0 += 24;
			iq[jz] = (int)fw;
		}
		else
		{
			iq[jz] = (int)z;
		}
	}

	/* convert integer "bit" chunk to floating-point value */
	fw = nolibc::_scalbn(1.0 , q0);
	for (int i = jz; i >= 0; i --)
	{
		q[i] = fw * (double)iq[i];
		fw *= 0x1p-24;
	}

	/* compute PIO2[0,...,jp]*q[jz,...,0] */
	for (int i = jz; i >= 0; i--)
	{
		for (fw = 0.0, int k = 0; k <= jp && k <= jz-i; k++) fw += PIO2[k] * q[i + k];
		fq[jz - i] = fw;
	}

	/* compress fq[] into y[] */
	switch (prec)
	{
		case 0:
			fw = 0.0;
			for (int i = jz; i >= 0; i--) fw += fq[i];
			y[0] = ih == 0 ? fw : -fw;
		case 1:
		case 2:
			fw = 0.0;
			for (int i = jz; i >= 0; i--)
			fw += fq[i];
			// TODO: drop excess precision here once double_t is used
			fw = (double)fw;
			y[0] = ih == 0 ? fw : -fw;
			fw = fq[0]-fw;
			for (int i = 1; i <= jz; i++) fw += fq[i];
			y[1] = ih == 0 ? fw : -fw;
		case 3:  /* painful */
			for (int i = jz; i > 0; i--)
			{
				fw = fq[i - 1] + fq[i];
				fq[i] += fq[i - 1] - fw;
				fq[i - 1] = fw;
			}
			for (int i = jz; i > 1; i--)
			{
				fw = fq[i-1] + fq[i];
				fq[i] += fq[i - 1] - fw;
				fq[i - 1] = fw;
			}
			for (fw = 0.0, int i = jz; i >= 2; i--)
			fw += fq[i];
			if (ih == 0)
			{
				y[0] = fq[0];
				y[1] = fq[1];
				y[2] = fw;
			}
			else
			{
				y[0] = -fq[0];
				y[1] = -fq[1];
				y[2] = -fw;
			}
	}
	return n & 7;
}

/* origin: FreeBSD /usr/src/lib/msun/src/e_rem_pio2.c */
/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunSoft, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================
 *
 * Optimized by Bruce D. Evans.
 */
/*
 * invpio2:  53 bits of 2/pi
 * pio2_1:   first  33 bit of pi/2
 * pio2_1t:  pi/2 - pio2_1
 * pio2_2:   second 33 bit of pi/2
 * pio2_2t:  pi/2 - (pio2_1+pio2_2)
 * pio2_3:   third  33 bit of pi/2
 * pio2_3t:  pi/2 - (pio2_1+pio2_2+pio2_3)
 */

/* caller must handle the case when reduction is not needed: |x| ~<= pi/4 */
fn int __rem_pio2(double x, double *y)
{
	const double PIO4 = 0x1.921fb54442d18p-1;
    const double INVPIO2 = 6.36619772367581382433e-01; /* 0x3FE45F30, 0x6DC9C883 */
    const double PIO2_1 = 1.57079632673412561417e+00; /* 0x3FF921FB, 0x54400000 */
    const double PIO2_1T = 6.07710050650619224932e-11; /* 0x3DD0B461, 0x1A626331 */
    const double PIO2_2 = 6.07710050630396597660e-11; /* 0x3DD0B461, 0x1A600000 */
    const double PIO2_2T = 2.02226624879595063154e-21; /* 0x3BA3198A, 0x2E037073 */
    const double PIO2_3 = 2.02226624871116645580e-21; /* 0x3BA3198A, 0x2E000000 */
    const double PIO2_3T = 8.47842766036889956997e-32; /* 0x397B839A, 0x252049C1 */

	ulong u = bitcast(x, ulong);
	int sign = (int)(u >> 63);
	uint ix = (uint)(u >> 32 & 0x7fffffff);
	switch
	{
		// |x| ~<= 5pi/4
		case ix <= 0x400f6a7a:
			// |x| ~= pi/2 or 2pi/2
			if ((ix & 0xfffff) == 0x921fb)
			{
				// cancellation -- use medium case
				break;
			}
			// |x| ~<= 3pi/4
			if (ix <= 0x4002d97c)
			{
				if (!sign)
				{
					double z = x - PIO2_1;  /* one round good to 85 bits */
					y[0] = z - PIO2_1T;
					y[1] = (z - y[0]) - PIO2_1T;
					return 1;
				}
				double z = x + PIO2_1;
				y[0] = z + PIO2_1T;
				y[1] = (z - y[0]) + PIO2_1T;
				return -1;
			}
			if (!sign)
			{
				double z = x - 2 * PIO2_1;
				y[0] = z - 2 * PIO2_1T;
				y[1] = (z - y[0]) - 2 * PIO2_1T;
				return 2;
			}
			double z = x + 2 * PIO2_1;
			y[0] = z + 2 * PIO2_1T;
			y[1] = (z - y[0]) + 2 * PIO2_1T;
			return -2;
		// |x| ~<= 9pi/4
		case ix <= 0x401c463b:
			// |x| ~<= 7pi/4
			if (ix <= 0x4015fdbc)
			{
				// |x| ~= 3pi/2
				if (ix == 0x4012d97c) break; // Use medium

				if (!sign)
				{
					double z = x - 3 * PIO2_1;
					y[0] = z - 3 * PIO2_1T;
					y[1] = (z - y[0]) - 3 * PIO2_1T;
					return 3;
				}
				double z = x + 3 * PIO2_1;
				y[0] = z + 3 * PIO2_1T;
				y[1] = (z - y[0]) + 3 * PIO2_1T;
				return -3;
			}
			// |x| ~= 4pi/2
			if (ix == 0x401921fb) break; // Use medium
			if (!sign)
			{
				double z = x - 4 * PIO2_1;
				y[0] = z - 4 * PIO2_1T;
				y[1] = (z - y[0]) - 4 * PIO2_1T;
				return 4;
			}
			double z = x + 4*PIO2_1;
			y[0] = z + 4 * PIO2_1T;
			y[1] = (z - y[0]) + 4 * PIO2_1T;
			return -4;
		case ix < 0x413921fb:
			break; // medium
		case ix >= 0x7ff00000:
		    // x is inf or NaN */
            y[0] = y[1] = x - x;
            return 0;
		default:
			// all other (large) arguments
        	// set z = scalbn(|x|,-ilogb(x)+23)
        	u = bitcast(x, ulong);
        	u &= ((ulong)-1) >> 12;
        	u |= (ulong)(0x3ff + 23) << 52;
        	double z = bitcast(u, double);
        	double[3] tx;
        	int i;
        	for (i = 0; i < 2; i++)
        	{
        		tx[i] = (double)(int)z;
        		z = (z - tx[i]) * 0x1p24;
        	}
        	tx[i] = z;
        	// skip zero terms, first term is non-zero
        	while (tx[i] == 0.0) i--;
        	double[2] ty;
        	int n = __rem_pio2_large(&tx, &ty, (int)(ix >> 20) - (0x3ff + 23), i + 1, 1);
        	if (sign)
        	{
        		y[0] = -ty[0];
        		y[1] = -ty[1];
        		return -n;
        	}
        	y[0] = ty[0];
        	y[1] = ty[1];
        	return n;
	}

	// |x| ~< 2^20*(pi/2), medium size
	// rint(x/(pi/2))
	double fnn = (double)x * INVPIO2 + TOINT15 - TOINT15;
	int n = (int)fnn;
	double r = x - fnn * PIO2_1;
	double w = fnn * PIO2_1T;  // 1st round, good to 85 bits
	// Matters with directed rounding.
	if (r - w < -PIO4) /*@unlikely*/
	{
		n--;
		fnn--;
		r = x - fnn * PIO2_1;
		w = fnn * PIO2_1T;
	}
	else if (r - w > PIO4) /*@unlikely*/
	{
		n++;
		fnn++;
		r = x - fnn * PIO2_1;
		w = fnn * PIO2_1T;
	}
	y[0] = r - w;
	u = bitcast(y[0], ulong);
	int ey = (int)(u >> 52 & 0x7ff);
	int ex = (int)(ix >> 20);
	if (ex - ey > 16)
	{
		// 2nd round, good to 118 bits */
		double t = r;
		w = fnn * PIO2_2;
		r = t - w;
		w = fnn * PIO2_2T - ((t - r) - w);
		y[0] = r - w;
		u = bitcast(y[0], ulong);
		ey = (int)(u >> 52 & 0x7ff);
		if (ex - ey > 49)
		{
			// 3rd round, good to 151 bits, covers all cases
			t = r;
			w = fnn * PIO2_3;
			r = t - w;
			w = fnn * PIO2_3T - ((t - r) - w);
			y[0] = r - w;
		}
	}
	y[1] = (r - y[0]) - w;
	return n;


}
