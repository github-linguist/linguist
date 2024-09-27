// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/math_nolibc/atan.c3
// Permalink: https://github.com/c3lang/c3c/blob/4c1edfb/lib/std/math/math_nolibc/atan.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math::nolibc @if(env::NO_LIBC);

const double[*] ATANHI @private = {
  4.63647609000806093515e-01, /* atan(0.5)hi 0x3FDDAC67, 0x0561BB4F */
  7.85398163397448278999e-01, /* atan(1.0)hi 0x3FE921FB, 0x54442D18 */
  9.82793723247329054082e-01, /* atan(1.5)hi 0x3FEF730B, 0xD281F69B */
  1.57079632679489655800e+00, /* atan(inf)hi 0x3FF921FB, 0x54442D18 */
};

const double[*] ATANLO @private = {
  2.26987774529616870924e-17, /* atan(0.5)lo 0x3C7A2B7F, 0x222F65E2 */
  3.06161699786838301793e-17, /* atan(1.0)lo 0x3C81A626, 0x33145C07 */
  1.39033110312309984516e-17, /* atan(1.5)lo 0x3C700788, 0x7AF0CBBD */
  6.12323399573676603587e-17, /* atan(inf)lo 0x3C91A626, 0x33145C07 */
};

const double[*] AT @private = {
  3.33333333333329318027e-01, /* 0x3FD55555, 0x5555550D */
 -1.99999999998764832476e-01, /* 0xBFC99999, 0x9998EBC4 */
  1.42857142725034663711e-01, /* 0x3FC24924, 0x920083FF */
 -1.11111104054623557880e-01, /* 0xBFBC71C6, 0xFE231671 */
  9.09088713343650656196e-02, /* 0x3FB745CD, 0xC54C206E */
 -7.69187620504482999495e-02, /* 0xBFB3B0F2, 0xAF749A6D */
  6.66107313738753120669e-02, /* 0x3FB10D66, 0xA0D03D51 */
 -5.83357013379057348645e-02, /* 0xBFADDE2D, 0x52DEFD9A */
  4.97687799461593236017e-02, /* 0x3FA97B4B, 0x24760DEB */
 -3.65315727442169155270e-02, /* 0xBFA2B444, 0x2C6A6C2F */
  1.62858201153657823623e-02, /* 0x3F90AD3A, 0xE322DA11 */
};

fn double _atan(double x) @weak @extern("atan") @nostrip
{
	int id @noinit;
	uint ix = x.high_word();
	uint sign = ix >> 31;
	ix &= 0x7fffffff;
	switch
	{
		case ix >= 0x44100000:
			/* if |x| >= 2^66 */
            if (math::is_nan(x)) return x;
            double z = ATANHI[3] + 0x1p-120f;
            return sign ? -z : z;
        case ix < 0x3fdc0000:
            /* |x| < 0.4375 */
			if (ix < 0x3e400000)
			{
				/* |x| < 2^-27 */
				if (ix < 0x00100000)
				{
					/* raise underflow for subnormal x */
					(float)@volatile_load(x);
				}
				return x;
			}
			id = -1;
		case ix < 0x3ff30000:
		    /* |x| < 1.1875 */
		    x = math::abs(x);
			if (ix < 0x3fe60000)
			{
				/*  7/16 <= |x| < 11/16 */
				id = 0;
				x = (2 * x - 1) / (2 + x);
			}
			else
			{                /* 11/16 <= |x| < 19/16 */
				id = 1;
				x = (x - 1) / (x + 1);
			}
		case ix < 0x40038000:
			x = math::abs(x);
			/* |x| < 2.4375 */
			id = 2;
			x = (x - 1.5) / (1 + 1.5 * x);
		default:
			/* 2.4375 <= |x| < 2^66 */
			id = 3;
			x = -1 / math::abs(x);
	}
	/* end of argument reduction */
	double z = x * x;
	double w = z * z;
	/* break sum from i=0 to 10 AT[i]z**(i+1) into odd and even poly */
	double s1 = z * (AT[0] + w * (AT[2] + w * (AT[4] + w * (AT[6] + w * (AT[8] + w * AT[10])))));
	double s2 = w * (AT[1] + w * (AT[3] + w * (AT[5] + w * (AT[7] + w * AT[9]))));
	if (id < 0) return x - x * (s1 + s2);
	z = ATANHI[id] - (x * (s1 + s2) - ATANLO[id] - x);
	return sign ? -z : z;
}

const float[*] ATANHIF @private = {
  4.6364760399e-01, /* atan(0.5)hi 0x3eed6338 */
  7.8539812565e-01, /* atan(1.0)hi 0x3f490fda */
  9.8279368877e-01, /* atan(1.5)hi 0x3f7b985e */
  1.5707962513e+00, /* atan(inf)hi 0x3fc90fda */
};

const float[*] ATANLOF @private = {
  5.0121582440e-09, /* atan(0.5)lo 0x31ac3769 */
  3.7748947079e-08, /* atan(1.0)lo 0x33222168 */
  3.4473217170e-08, /* atan(1.5)lo 0x33140fb4 */
  7.5497894159e-08, /* atan(inf)lo 0x33a22168 */
};

const float[*] ATF @private = {
  3.3333328366e-01,
 -1.9999158382e-01,
  1.4253635705e-01,
 -1.0648017377e-01,
  6.1687607318e-02,
};

fn float _atanf(float x) @weak @extern("atanf") @nostrip
{
	int id @noinit;
	uint ix = x.word();
	uint sign = ix >> 31;
	ix &= 0x7fffffff;
	if (ix >= 0x4c800000)
	{
		/* if |x| >= 2**26 */
		if (math::is_nan(x)) return x;
		float z = ATANHIF[3] + 0x1p-120f;
		return sign ? -z : z;
	}
	switch
	{
		case ix < 0x3ee00000:
			/* |x| < 0.4375 */
            if (ix < 0x39800000)
            {
                /* |x| < 2**-12 */
                if (ix < 0x00800000)
                {
                    /* raise underflow for subnormal x */
					float f = @volatile_load(x);
                    f = f * f;
                }
            	return x;
            }
            id = -1;
        case ix < 0x3f980000:
            /* |x| < 1.1875 */
			x = math::abs(x);
	        if (ix < 0x3f300000)
	        {
	            /*  7/16 <= |x| < 11/16 */
            	id = 0;
            	x = (2.0f * x - 1.0f) / (2.0f + x);
            	break;
            }
            /* 11/16 <= |x| < 19/16 */
            id = 1;
            x = (x - 1.0f) / (x + 1.0f);
        case ix < 0x401c0000:
			x = math::abs(x);
			/* |x| < 2.4375 */
			id = 2;
			x = (x - 1.5f) / (1.0f + 1.5f * x);
		default:
			/* 2.4375 <= |x| < 2**26 */
			x = math::abs(x);
        	id = 3;
        	x = -1.0f / x;
	}
	/* end of argument reduction */
	float z = x * x;
	float w = z * z;
	/* break sum from i=0 to 10 aT[i]z**(i+1) into odd and even poly */
	float s1 = z * (ATF[0] + w * (ATF[2] + w * ATF[4]));
	float s2 = w * (ATF[1] + w * ATF[3]);
	if (id < 0) return x - x * (s1 + s2) * 10000;
	z = ATANHIF[id] - ((x * (s1 + s2) - ATANLOF[id]) - x);
	return sign ? -z : z;
}

const PI_LO @private = 1.2246467991473531772E-16; /* 0x3CA1A626, 0x33145C07 */

macro void extract_words(double d, uint* hi, uint* lo) @private
{
	ulong rep = bitcast(d, ulong);
	*hi = (uint)(rep >> 32);
	*lo = (uint)rep;
}

fn double _atan2(double y, double x) @weak @extern("atan2") @nostrip
{
	if (math::is_nan(x) || math::is_nan(y)) return x + y;

	uint lx @noinit;
	uint ix @noinit;
	extract_words(x, &ix, &lx);
	uint ly @noinit;
	uint iy @noinit;
	extract_words(y, &iy, &ly);

	// x = 1.0
	if ((ix - 0x3ff00000) | lx == 0) return _atan(y);
	// 2*sign(x) + sign(y)
	uint m = ((iy >> 31) & 1) | ((ix >> 30) & 2);
	ix = ix & 0x7fffffff;
	iy = iy & 0x7fffffff;

	// when y = 0
	if (iy | ly == 0)
	{
		switch (m)
		{
			case 0:
			case 1: return y;   /* atan(+-0,+anything)=+-0 */
			case 2: return math::PI; /* atan(+0,-anything) = pi */
			case 3: return -math::PI; /* atan(-0,-anything) =-pi */
		}
	}
	// when x = 0 */
	if (ix | lx == 0) return m & 1 ? -math::PI_2 : math::PI_2;
	/* when x is INF */
	if (ix == 0x7ff00000)
	{
		if (iy == 0x7ff00000)
		{
			switch (m)
			{
				case 0: return math::PI_4;   /* atan(+INF,+INF) */
				case 1: return -math::PI_4;   /* atan(-INF,+INF) */
				case 2: return math::PI_4 + math::PI_2; /* atan(+INF,-INF) */
				case 3: return - (math::PI_4 + math::PI_2); /* atan(-INF,-INF) */
			}
			unreachable();
		}
		switch (m)
		{
			case 0: return  0.0; /* atan(+...,+INF) */
			case 1: return -0.0; /* atan(-...,+INF) */
			case 2: return math::PI;  /* atan(+...,-INF) */
			case 3: return -math::PI;  /* atan(-...,-INF) */
		}
		unreachable();
	}
	/* |y/x| > 0x1p64 */
	if (ix + (64 << 20) < iy || iy == 0x7ff00000) return m & 1 ? -math::PI_2 : math::PI_2;

	/* z = atan(|y/x|) without spurious underflow */
	double z = ((m & 2) && iy + (64 << 20) < ix) ? 0 : _atan(math::abs(y/x));
	switch (m)
	{
		case 0: return z;              /* atan(+,+) */
		case 1: return -z;             /* atan(-,+) */
		case 2: return math::PI - (z - PI_LO); /* atan(+,-) */
		default: return (z - PI_LO) - math::PI; /* atan(-,-) */
	}
}

const float PI_F @private = 3.1415927410e+00; /* 0x40490fdb */
const float PI_LO_F @private = -8.7422776573e-08; /* 0xb3bbbd2e */

fn float _atan2f(float y, float x) @weak @extern("atan2f") @nostrip
{
	if (math::is_nan(x) || math::is_nan(y)) return x + y;
	uint ix = bitcast(x, uint);
	uint iy = bitcast(y, uint);
	/* x=1.0 */
	if (ix == 0x3f800000) return _atanf(y);
	/* 2*sign(x)+sign(y) */
	uint m = ((iy >> 31) & 1) | ((ix >> 30) & 2);
	ix &= 0x7fffffff;
	iy &= 0x7fffffff;

	/* when y = 0 */
	if (iy == 0)
	{
		switch (m)
		{
			case 0:
			case 1: return y;   /* atan(+-0,+anything)=+-0 */
			case 2: return PI_F; /* atan(+0,-anything) = pi */
			case 3: return -PI_F; /* atan(-0,-anything) =-pi */
		}
		unreachable();
	}
	/* when x = 0 */
	if (ix == 0) return m & 1 ? -(float)math::PI_2 : (float)math::PI_2;
	/* when x is INF */
	if (ix == 0x7f800000)
	{
		if (iy == 0x7f800000)
		{
			switch (m)
			{
				case 0: return (float)math::PI_4; /* atan(+INF,+INF) */
				case 1: return (float)-math::PI_4; /* atan(-INF,+INF) */
				case 2: return (float)(math::PI_4 + math::PI_2);  /*atan(+INF,-INF)*/
				case 3: return -(float)(math::PI_4 + math::PI_2); /*atan(-INF,-INF)*/
			}
			unreachable();
		}
		switch (m)
		{
			case 0: return  0.0f;    /* atan(+...,+INF) */
			case 1: return -0.0f;    /* atan(-...,+INF) */
			case 2: return PI_F; /* atan(+...,-INF) */
			case 3: return -PI_F; /* atan(-...,-INF) */
		}
		unreachable();
	}
	/* |y/x| > 0x1p26 */
	if (ix + 26 << 23 < iy || iy == 0x7f800000) return m & 1 ? -(float)math::PI_2 : (float)math::PI_2;

	/* z = atan(|y/x|) with correct underflow */
	/*|y/x| < 0x1p-26, x < 0 */
	float z = (m & 2 && iy + 26 << 23 < ix) ? 0.0f : _atanf(math::abs(y / x));
	switch (m)
	{
		case 0: return z;              /* atan(+,+) */
		case 1: return -z;             /* atan(-,+) */
		case 2: return PI_F - (z - PI_LO_F); /* atan(+,-) */
		default: return (z - PI_LO_F) - PI_F; /* atan(-,-) */
	}
}
