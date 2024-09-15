// Source: https://github.com/c3lang/c3c/blob/master/lib/std/core/string_to_real.c3
// Permalink: https://github.com/c3lang/c3c/blob/499c82b/lib/std/core/string_to_real.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::core::string;
import std::math;

// Float parsing based on code in Musl floatscan.c by Rich Felker.
// Musl uses the MIT license, copied below:
// ----------------------------------------------------------------------
//   Copyright © 2005-2014 Rich Felker, et al.
//
//   Permission is hereby granted, free of charge, to any person obtaining
//   a copy of this software and associated documentation files (the
//   "Software"), to deal in the Software without restriction, including
//   without limitation the rights to use, copy, modify, merge, publish,
//   distribute, sublicense, and/or sell copies of the Software, and to
//   permit persons to whom the Software is furnished to do so, subject to
//   the following conditions:
//
//   The above copyright notice and this permission notice shall be
//   included in all copies or substantial portions of the Software.
//
//   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
//   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
//   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
//   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
//   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
//   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
//   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//   ----------------------------------------------------------------------

const KMAX = 128;
const MASK = KMAX - 1;
const B1B_DIG = 2;
const uint[2] B1B_MAX = { 9007199, 254740991 };

/**
 * @require chars.len > 0
 **/
macro double! decfloat(char[] chars, int $bits, int $emin, int sign)
{
	uint[KMAX] x;
	const uint[2] TH = B1B_MAX;
	int emax = - $emin - $bits + 3;

	const int[*] P10S = { 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000 };
	usz index;
	bool got_digit = chars[0] == '0';
	bool got_rad;
	long lrp, dc;
	int k, j, lnz;
	usz len = chars.len;
	usz last_char = len - 1;

	assert(len);

	char c @noinit;
	// Skip past first characters
	while ((c = chars[index]) == '0')
	{
		if (index == last_char) return sign * 0.0;
		index++;
	}

	if (c == '.')
	{
		got_rad = true;
		if (index == last_char)
		{
			if (!got_digit) return NumberConversion.MALFORMED_FLOAT?;
			return sign * 0.0;
		}
		if (index != last_char && (c = chars[++index]) == '0')
		{
			lrp--;
			got_digit = true;
			while (last_char != index && (c = chars[++index]) == '0')
			{
				lrp--;
			}
		}
	}

	while (c - '0' < 10u || c == '.')
	{
		switch
		{
			case c == '.':
				if (got_rad) return NumberConversion.MALFORMED_FLOAT?;
				got_rad = true;
				lrp = dc;
			case k < KMAX - 3:
				dc++;
				if (c != '0') lnz = (int)dc;
				if (j)
				{
					x[k] = x[k] * 10 + c - '0';
				}
				else
				{
					x[k] = c - '0';
				}
				if (++j == 9)
				{
					k++;
					j = 0;
				}
				got_digit = true;
			default:
				dc++;
				if (c != '0') x[KMAX - 4] |= 1;

		}
		if (index == last_char) break;
		assert(index < last_char);
		c = chars[++index];
	}
	if (!got_rad) lrp = dc;
	if (!got_digit) return NumberConversion.MALFORMED_FLOAT?;
	if ((c | 32) == 'e')
	{
		if (last_char == index) return NumberConversion.MALFORMED_FLOAT?;
		long e10 = String.to_long((String)chars[index + 1..]) ?? NumberConversion.MALFORMED_FLOAT?!;
		lrp += e10;
	}
	else if (index != last_char)
	{
		return NumberConversion.MALFORMED_FLOAT?;
	}
	// Handle zero specially to avoid nasty special cases later
	if (!x[0]) return sign * 0.0;

	// Optimize small integers (w/no exponent) and over/under-flow
	if (lrp == dc && dc < 10 && ($bits > 30 || (ulong)x[0] >> $bits == 0)) return sign * (double)x[0];
	if (lrp > - $emin / 2) return NumberConversion.FLOAT_OUT_OF_RANGE?;
	if (lrp < $emin - 2 * math::DOUBLE_MANT_DIG) return NumberConversion.FLOAT_OUT_OF_RANGE?;

	// Align incomplete final B1B digit
	if (j)
	{
		for (; j < 9; j++) x[k] *= 10;
		k++;
		j = 0;
	}

	int a;
	int z = k;
	int e2;
	long rp = lrp;

	// Optimize small to mid-size integers (even in exp. notation)
	if (lnz < 9 && lnz <= rp && rp < 18)
	{
		if (rp == 9) return sign * (double)x[0];
		if (rp < 9) return sign * (double)x[0] / P10S[8 - rp];
		int bitlim = $bits - 3 * (int)(rp - 9);
		if (bitlim > 30 || x[0] >> bitlim == 0) return sign * (double)x[0] * P10S[rp - 10];
	}

	// Align radix point to B1B digit boundary
	if (rp % 9)
	{
		long rpm9 = rp >= 0 ? rp % 9 : rp % 9 + 9;
		int p10 = P10S[8 - rpm9];
		uint carry = 0;
		for (k = a; k != z; k++)
		{
			uint tmp = x[k] % p10;
			x[k] = x[k] / p10 + carry;
			carry = 1000000000 / p10 * tmp;
			if (k == a && !x[k])
			{
				a = (a + 1) & MASK;
				rp -= 9;
			}
		}
		if (carry) x[z++] = carry;
		rp += 9 - rpm9;
	}

	// Upscale until desired number of bits are left of radix point
	while (rp < 9 * B1B_DIG || (rp == 9 * B1B_DIG && x[a] < TH[0]))
	{
		uint carry = 0;
		e2 -= 29;
		for (k = (z - 1) & MASK; ; k = (k - 1) & MASK)
		{
			ulong tmp = (ulong)x[k] << 29 + carry;
			if (tmp > 1000000000)
			{
				carry = (uint)(tmp / 1000000000);
				x[k] = (uint)(tmp % 1000000000);
			}
			else
			{
				carry = 0;
				x[k] = (uint)tmp;
			}
			if (k == (z - 1) & MASK && k != a && !x[k]) z = k;
			if (k == a) break;
		}
		if (carry)
		{
			rp += 9;
			a = (a - 1) & MASK;
			if (a == z)
			{
				z = (z - 1) & MASK;
				x[(z - 1) & MASK] |= x[z];
			}
			x[a] = carry;
		}
	}

	// Downscale until exactly number of bits are left of radix point
	while (true)
	{
		uint carry = 0;
		int sh = 1;
		int i;
		for (i = 0; i < B1B_DIG; i++)
		{
			k = (a + i) & MASK;
			if (k == z || x[k] < TH[i])
			{
				i = B1B_DIG;
				break;
			}
			if (x[(a + i) & MASK] > TH[i]) break;
		}
		if (i == B1B_DIG && rp == 9 * B1B_DIG) break;
		if (rp > 9 + 9 * B1B_DIG) sh = 9;
		e2 += sh;
		for (k = a; k != z; k = (k+1) & MASK)
		{
			uint tmp = x[k] & (1 << sh - 1);
			x[k] = x[k] >> sh + carry;
			carry = (1000000000 >> sh) * tmp;
			if (k == a && !x[k])
			{
				a = (a + 1) & MASK;
				i--;
				rp -= 9;
			}
		}
		if (carry)
		{
			if ((z + 1) & MASK != a)
			{
				x[z] = carry;
				z = (z + 1) & MASK;
			}
			else
			{
				x[(z - 1) & MASK] |= 1;
			}
		}
	}

	// Assemble desired bits into floating point variable
	double y;
	int i;
	for (i = 0; i < B1B_DIG; i++)
	{
		if ((a + i) & MASK == z) x[(z = (z + 1) & MASK) - 1] = 0;
		y = 1000000000.0 * y + x[(a + i) & MASK];
	}

	y *= sign;

	bool denormal;
	/* Limit precision for denormal results */
	uint bits = $bits;
	if (bits > math::DOUBLE_MANT_DIG + e2 - $emin)
	{
		bits = math::DOUBLE_MANT_DIG + e2 - $emin;
		if (bits < 0) bits = 0;
		denormal = true;
	}

	// Calculate bias term to force rounding, move out lower bits
	double bias;
	double frac;
	if (bits < math::DOUBLE_MANT_DIG)
	{
		bias = math::copysign(math::scalbn(1, 2 * math::DOUBLE_MANT_DIG - bits - 1), y);
		frac = y % math::scalbn(1, math::DOUBLE_MANT_DIG - bits);
		y -= frac;
		y += bias;
	}

	// Process tail of decimal input so it can affect rounding
	if ((a + i) & MASK != z)
	{
		uint t = x[(a + i) & MASK];
		switch
		{
			case t < 500000000 && (t || (a + i + 1) & MASK != z):
				frac += 0.25 * sign;
			case t > 500000000:
				frac += 0.75 * sign;
			case t == 500000000:
				if ((a + i + 1) & MASK == z)
				{
					frac += 0.5 * sign;
				}
				else
				{
					frac += 0.75 * sign;
				}
		}
		if (math::DOUBLE_MANT_DIG - bits >= 2 && !(frac % 1)) frac++;
	}

	y += frac;
	y -= bias;

	if (((e2 + math::DOUBLE_MANT_DIG) & int.max) > emax - 5)
	{
		if (math::abs(y) >= 0x1p53)
		{
			if (denormal && bits == math::DOUBLE_MANT_DIG + e2 - $emin) denormal = false;
			y *= 0.5;
			e2++;
		}
		if (e2 + math::DOUBLE_MANT_DIG > emax || (denormal && frac)) return NumberConversion.MALFORMED_FLOAT?;
	}
	return math::scalbn(y, e2);
}

macro double! hexfloat(char[] chars, int $bits, int $emin, int sign)
{
	double scale = 1;
	uint x;
	long rp;
	long dc;
	char c @noinit;
	bool got_rad;
	bool got_digit;
	bool got_tail;
	usz len = chars.len;
	usz last_char = len - 1;
	usz index;
	double y;

	// Skip past first characters
	while ((c = chars[index]) == '0')
	{
		if (index == last_char) return 0.0;
		index++;
	}
	if (c == '.')
	{
		got_rad = true;
		if (index == last_char)
		{
			if (!got_digit) return NumberConversion.MALFORMED_FLOAT?;
			return sign * 0.0;
		}
		if (index != last_char && (c = chars[++index]) == '0')
		{
			rp--;
			got_digit = true;
			while (last_char != index && (c = chars[++index]) == '0')
			{
				rp--;
			}
		}
	}

	while ((c - '0') < 10u || ((c | 32) - 'a') < 6u || c == '.')
	{
		if (c == '.')
		{
			if (got_rad) return NumberConversion.MALFORMED_FLOAT?;
			got_rad = true;
			rp = dc;
		}
		else
		{
			got_digit = true;
			int d = {|
				if (c > '9') return (c | 32) + 10 - 'a';
				return c - '0';
			|};
			switch
			{
				case dc < 8:
					x = x * 16 + d;
				case dc < math::DOUBLE_MANT_DIG / 4 + 1:
					y += d * (scale /= 16);
					got_tail = true;
				case d && !got_tail:
					y += 0.5 * scale;
					got_tail = true;
			}
			dc++;
		}
		if (index == last_char) break;
		c = chars[++index];
	}
	if (!got_digit) return NumberConversion.MALFORMED_FLOAT?;
	if (!got_rad) rp = dc;
	for (; dc < 8; dc++) x *= 16;

	long e2;
	if ((c | 32) == 'p')
	{
		long e2val = String.to_long((String)chars[index + 1..]) ?? (NumberConversion.MALFORMED_FLOAT?)!;
		e2 = e2val;
	}
	e2 += 4 * rp - 32;
	if (!x) return sign * 0.0;
	if (e2 > -$emin) return NumberConversion.FLOAT_OUT_OF_RANGE?;
	if (e2 < $emin - 2 * math::DOUBLE_MANT_DIG) return NumberConversion.FLOAT_OUT_OF_RANGE?;

	while (x < 0x80000000)
	{
		if (y >= 0.5)
		{
			x += x + 1;
			y += y - 1;
		}
		else
		{
			x += x;
			y += y;
		}
		e2--;
	}
	int bits = $bits;
	if ($bits > 32 + e2 - $emin)
	{
		bits = (int)(32 + e2 - $emin);
		if (bits < 0) bits = 0;
	}
	double bias;
	if (bits < math::DOUBLE_MANT_DIG)
	{
		bias = math::copysign(math::scalbn(1, 32 + math::DOUBLE_MANT_DIG - bits - 1), (double)sign);
	}

	if (bits < 32 && y && !(x & 1))
	{
		x++;
		y = 0;
	}
	y = bias + sign * (double)x + sign * y;
	y -= bias;
	if (!y) return NumberConversion.FLOAT_OUT_OF_RANGE?;

	return math::scalbn(y, (int)e2);
}

macro String.to_real(chars, $Type) @private
{
	int sign = 1;
	$switch ($Type)
	$case float:
		const int BITS = math::FLOAT_MANT_DIG;
		const int EMIN = math::FLOAT_MIN_EXP - BITS;
	$case double:
		const int BITS = math::DOUBLE_MANT_DIG;
		const int EMIN = math::DOUBLE_MIN_EXP - BITS;
	$case float128:
		$error "Not yet supported";
	$default:
		$error "Unexpected type";
	$endswitch

	while (chars.len && chars[0] == ' ') chars = chars[1..];
	if (!chars.len) return NumberConversion.MALFORMED_FLOAT?;
	switch (chars[0])
	{
		case '-':
			sign = -1;
			nextcase;
		case '+':
			chars = chars[1..];
	}
	if (chars == "infinity" || chars == "INFINITY") return sign * $Type.inf;
	if (chars == "NAN" || chars == "nan") return $Type.nan;

	if (chars.len > 2 && chars[0] == '0' && (chars[1] | 32) == 'x')
	{
		return ($Type)hexfloat((char[])chars[2..], BITS, EMIN, sign);
	}
	return ($Type)decfloat((char[])chars, BITS, EMIN, sign);
}

