// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/math_i128.c3
// Permalink: https://github.com/c3lang/c3c/blob/26acce2/lib/std/math/math_i128.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math;

fn int128 __divti3(int128 a, int128 b) @extern("__divti3") @weak @nostrip
{
	int128 sign_a = a >> 127; // -1 : 0
	int128 sign_b = b >> 127; // -1 : 0
	uint128 unsigned_a = (uint128)(a ^ sign_a) + (-sign_a);
	uint128 unsigned_b = (uint128)(b ^ sign_b) + (-sign_b);
	sign_a ^= sign_b; // quotient sign
	return __udivti3(unsigned_a, unsigned_b) @inline ^ sign_a + (-sign_a);
}

macro uint128 @__udivmodti4(uint128 a, uint128 b, bool $return_rem)
{
	Int128bits n = { .all = a };
	Int128bits d = { .all = b };
	Int128bits q @noinit;
	Int128bits r @noinit;
	uint sr;
	if (n.high == 0)
	{
		if (d.high == 0)
		{
			$if $return_rem:
				return n.low % d.low;
			$else
				return n.low / d.low;
			$endif
		}
		$if $return_rem:
			return n.low;
		$else
			return 0;
		$endif
	}
	if (d.low == 0)
	{
		if (d.high == 0)
		{
			$if $return_rem:
				return n.high % d.low;
			$else
				return n.high / d.low;
			$endif
		}
		if (n.low == 0)
		{
			$if $return_rem:
				r.high = n.high % d.high;
				r.low = 0;
				return r.all;
			$else
				return n.high / d.high;
			$endif
		}
		if (d.high & (d.high - 1) == 0) // d is pot
		{
			$if $return_rem:
				r.low = n.low;
				r.high = n.high & (d.high - 1);
				return r.all;
			$else
				return (uint128)(n.high >> $$ctz(d.high));
			$endif
        }
        sr = (uint)$$clz(d.high) - (uint)$$clz(n.high);
        // 0 <= sr <= n_udword_bits - 2 or sr large
        if (sr > 64 - 2)
        {
            $if $return_rem:
                return n.all;
            $else
                return 0;
            $endif
        }
        sr++;
        // 1 <= sr <= n_udword_bits - 1
        // q.all = n.all << (n_utword_bits - sr);
        q.low = 0;
        q.high = n.low << (64 - sr);
        r.high = n.high >> sr;
        r.low = (n.high << (64 - sr)) | (n.low >> sr);
    }
    else // d.s.low != 0
    {
        if (d.high == 0)
        {
            if (d.low & (d.low - 1) == 0) // if d is a power of 2
            {
                $if $return_rem:
                    return (uint128)(n.low & (d.low - 1));
                $else
                    if (d.low == 1) return n.all;
                    sr = (uint)$$ctz(d.low);
                    q.high = n.high >> sr;
                    q.low = (n.high << (64 - sr)) | (n.low >> sr);
                    return q.all;
               $endif
            }
            sr = 1 + 64 + (uint)$$clz(d.low) - (uint)$$clz(n.high);
            // 2 <= sr <= n_utword_bits - 1
            // q.all = n.all << (n_utword_bits - sr);
            // r.all = n.all >> sr;
            switch
            {
                case sr == 64:
					q.low = 0;
					q.high = n.low;
					r.high = 0;
					r.low = n.high;
				case sr < 64:
                    q.low = 0;
                    q.high = n.low << (64 - sr);
                    r.high = n.high >> sr;
                    r.low = (n.high << (64 - sr)) | (n.low >> sr);
                default: // n_udword_bits + 1 <= sr <= n_utword_bits - 1
                    q.low = n.low << (128 - sr);
                    q.high = (n.high << (128 - sr)) | (n.low >> (sr - 64));
                    r.high = 0;
                    r.low = n.high >> (sr - 64);
            }
        }
        else
        {
			sr = (uint)$$clz(d.high) - (uint)$$clz(n.high);
            // 0 <= sr <= n_udword_bits - 1 or sr large
            if (sr > 64 - 1)
            {
                $if $return_rem:
                    return n.all;
                $else
                    return 0;
                $endif
            }

            sr++;
			// 1 <= sr <= n_udword_bits
			// q.all = n.all << (n_utword_bits - sr);
			// r.all = n.all >> sr;
			q.low = 0;
            if (sr == 64)
            {
                q.high = n.low;
                r.high = 0;
                r.low = n.high;
			}
			else
			{
				r.high = n.high >> sr;
				r.low = (n.high << (64 - sr)) | (n.low >> sr);
				q.high = n.low << (64 - sr);
            }
        }
    }
    // Not a special case
    // q and r are initialized with:
    // q.all = n.all << (128 - sr);
    // r.all = n.all >> sr;
    // 1 <= sr <= n_utword_bits - 1
    uint carry = 0;
    for (; sr > 0; sr--)
    {
        // r:q = ((r:q)  << 1) | carry
        r.high = (r.high << 1) | (r.low  >> (64 - 1));
        r.low  = (r.low  << 1) | (q.high >> (64 - 1));
        q.high = (q.high << 1) | (q.low  >> (64 - 1));
        q.low  = (q.low  << 1) | carry;
        // carry = 0;
        // if (r.all >= d.all)
        // {
        //     r.all -= d.all;
        //      carry = 1;
        // }
        int128 s = (int128)(d.all - r.all - 1) >> (128 - 1);
        carry = (uint)(s & 1);
        r.all -= d.all & s;
    }
    $if $return_rem:
		return r.all;
    $else
		return (q.all << 1) | carry;
    $endif
}

fn uint128 __umodti3(uint128 n, uint128 d) @extern("__umodti3") @weak @nostrip
{
	return @__udivmodti4(n, d, true);
}

fn uint128 __udivti3(uint128 n, uint128 d) @extern("__udivti3") @weak @nostrip
{
	return @__udivmodti4(n, d, false);
}

fn int128 __modti3(int128 a, int128 b) @extern("__modti3") @weak @nostrip
{
	int128 sign = b >> 127;
	uint128 unsigned_b = (uint128)(b ^ sign) + (-sign);
	sign = a >> 127;
	uint128 unsigned_a = (uint128)(a ^ sign) + (-sign);

	return __umodti3(unsigned_a, unsigned_b) ^ sign + (-sign);
}

union Int128bits @private
{
	struct
	{
		ulong low;
		ulong high;
	}
	uint128 all;
}

fn uint128 __lshrti3(uint128 a, uint b) @extern("__lshrti3") @weak @nostrip
{
	Int128bits result;
	result.all = a;
	if (b >= 64)
	{
		result.low = result.high >> (b - 64);
		result.high = 0;
	}
	else
	{
		if (b == 0) return a;
		result.low = (result.high << (64 - b)) | (result.low >> b);
		result.high  = result.high >> b;
	}
	return result.all;
}

fn int128 __ashrti3(int128 a, uint b) @extern("__ashrti3") @weak @nostrip
{
	Int128bits result;
	result.all = a;
	if (b >= 64)
	{
		result.low = result.high >> (b - 64);
		result.high = result.high >> 63;
	}
	else
	{
		if (b == 0) return a;
		result.low = result.high << (64 - b) | (result.low >> b);
		result.high  = result.high >> b;
	}
	return result.all;
}

fn int128 __ashlti3(int128 a, uint b) @extern("__ashlti3") @weak @nostrip
{
	Int128bits result;
	result.all = a;
	if (b >= 64)
	{
		result.low = 0;
		result.high = result.low << (b - 64);
	}
	else
	{
		if (b == 0) return a;
		result.high = (result.high << b) | (result.low >> (64 - b));
		result.low  = result.low << b;
	}
	return result.all;
}

// Returns: a * b

fn int128 __mulddi3(ulong a, ulong b) @private
{
	Int128bits r;
	const ulong LOWER_MASK = 0xffff_ffff;
	r.low = (a & LOWER_MASK) * (b & LOWER_MASK);
	ulong t = r.low >> 32;
	r.low &= LOWER_MASK;
	t += (a >> 32) * (b & LOWER_MASK);
	r.low += (t & LOWER_MASK) << 32;
	r.high = t >> 32;
	t = r.low >> 32;
	r.low &= LOWER_MASK;
	t += (b >> 32) * (a & LOWER_MASK);
	r.low += (t & LOWER_MASK) << 32;
	r.high += t >> 32;
	r.high += (a >> 32) * (b >> 32);
	return r.all;
}

fn int128 __multi3(int128 a, int128 b) @extern("__multi3") @weak @nostrip
{
	Int128bits x = { .all = a };
	Int128bits y = { .all = b };
	Int128bits r = { .all = __mulddi3(x.low, y.low) };
	r.high += x.high * y.low + x.low * y.high;
	return r.all;
}

fn float __floattisf(int128 a) @extern("__floattisf") @weak @nostrip => float_from_i128(float, a);
fn double __floattidf(int128 a) @extern("__floattidf") @weak @nostrip => float_from_i128(double, a);
fn float __floatuntisf(uint128 a) @extern("__floatuntisf") @weak @nostrip => float_from_u128(float, a);
fn double __floatuntidf(uint128 a) @extern("__floatuntidf") @weak @nostrip => float_from_u128(double, a);
fn uint128 __fixunsdfti(double a) @weak @extern("__fixunsdfti") @nostrip => fixuint(a);
fn uint128 __fixunssfti(float a) @weak @extern("__fixunssfti") @nostrip => fixuint(a);
fn int128 __fixdfti(double a) @weak @extern("__fixdfti") @nostrip => fixint(a);
fn int128 __fixsfti(float a) @weak @extern("__fixsfti") @nostrip => fixint(a);


macro float_from_i128($Type, a) @private
{
	var $Rep;
	$switch ($Type)
		$case double:
			$Rep = ulong;
			const MANT_DIG = DOUBLE_MANT_DIG;
			const SIGNIFICANT_BITS = 52;
			const EXP_BIAS = 1023;
			const MANTISSA_MASK = 0xFFFFF_FFFF_FFFFu64;
			const SIGN_BIT = 1u64 << 63;
		$case float:
			$Rep = uint;
			const MANT_DIG = FLOAT_MANT_DIG;
			const EXP_BIAS = 127;
			const SIGNIFICANT_BITS = 23;
			const MANTISSA_MASK = 0x7F_FFFFu32;
			const SIGN_BIT = 1u32 << 31;
		$case float16:
			$Rep = ushort;
			const MANT_DIG = HALF_MANT_DIG;
		$case float128:
			$Rep = uint128;
			const MANT_DIG = QUAD_MANT_DIG;
	$endswitch
	if (a == 0) return ($Type)0;
	// Grab and remove sign.
	int128 sign = a >> 127;
	a = (a ^ sign) - sign;
	int sd = 128 - (int)$$clz(a); // digits
	int e = sd - 1; // exponent
	if (sd > MANT_DIG)
	{
		switch (sd)
		{
			case MANT_DIG + 1:
				a <<= 1;
			case MANT_DIG + 2:
				break;
			default:
				a = (a >> (sd - (MANT_DIG + 2)))
					| (uint128)((a & ((uint128)(-1) >> ((128 + MANT_DIG + 2) - sd))) != 0);
		}
		a |= (uint128)((a & 4) != 0);
		a++;
		a >>= 2;
		if (a & (1i128 << MANT_DIG))
		{
			a >>= 1;
			e++;
		}
	}
	else
	{
		a <<= (MANT_DIG - sd);
	}
	return bitcast((($Rep)sign & SIGN_BIT) | ((($Rep)e + ($Rep)EXP_BIAS) << SIGNIFICANT_BITS) | (($Rep)a & ($Rep)MANTISSA_MASK), $Type);
}

macro float_from_u128($Type, a) @private
{
	var $Rep;
	$switch ($Type)
		$case double:
			$Rep = ulong;
			const MANT_DIG = DOUBLE_MANT_DIG;
			const SIGNIFICANT_BITS = 52;
			const EXP_BIAS = 1023;
			const MANTISSA_MASK = 0xFFFFF_FFFF_FFFFu64;
		$case float:
			$Rep = uint;
			const MANT_DIG = FLOAT_MANT_DIG;
			const EXP_BIAS = 127;
			const SIGNIFICANT_BITS = 23;
			const MANTISSA_MASK = 0x7F_FFFFu32;
		$case float16:
			$Rep = ushort;
			const MANT_DIG = HALF_MANT_DIG;
		$case float128:
			$Rep = uint128;
			const MANT_DIG = QUAD_MANT_DIG;
	$endswitch
	if (a == 0) return ($Type)0;
	int sd = 128 - (int)$$clz(a); // digits
	int e = sd - 1; // exponent
	if (sd > MANT_DIG)
	{
		switch (sd)
		{
			case MANT_DIG + 1:
				a <<= 1;
			case MANT_DIG + 2:
				break;
			default:
				a = (a >> (sd - (MANT_DIG + 2)))
					| (uint128)((a & ((uint128)(-1) >> ((128 + MANT_DIG + 2) - sd))) != 0);
		}
		a |= (uint128)((a & 4) != 0);
		a++;
		a >>= 2;
		if (a & (1i128 << MANT_DIG))
		{
			a >>= 1;
			e++;
		}
	}
	else
	{
		a <<= (MANT_DIG - sd);
	}
	return bitcast(((($Rep)e + ($Rep)EXP_BIAS) << SIGNIFICANT_BITS) | (($Rep)a & ($Rep)MANTISSA_MASK), $Type);
}


macro fixuint(a) @private
{
	var $Rep;
	$switch ($typeof(a))
		$case double:
			$Rep = ulong;
			const EXPONENT_BITS = 11;
			const SIGNIFICANT_BITS = 52;
		$case float:
			$Rep = uint;
			const EXPONENT_BITS = 8;
			const SIGNIFICANT_BITS = 23;
		$case float16:
			$Rep = ushort;
			const EXPONENT_BITS = 5;
			const SIGNIFICANT_BITS = 10;
		$case float128:
			$Rep = uint128;
			const EXPONENT_BITS = 15;
			const SIGNIFICANT_BITS = 112;
	$endswitch
	const $Rep MAX_EXPONENT = ($Rep)1 << EXPONENT_BITS - 1u;
	const $Rep EXPONENT_BIAS = MAX_EXPONENT >> 1u;
	const $Rep ONE_REP =EXPONENT_BIAS << SIGNIFICANT_BITS;
	const $Rep SIGN_BIT = ($Rep)1 << (EXPONENT_BITS + SIGNIFICANT_BITS);
	const $Rep ABS_MASK = SIGN_BIT - 1u;
	const $Rep IMPLICIT_BIT = ($Rep)1 << SIGNIFICANT_BITS;
	const $Rep SIGNIFICANT_MASK = IMPLICIT_BIT - 1u;
	const $Rep EXPONENT_MASK = ABS_MASK ^ SIGNIFICANT_MASK;
	const $Rep QUIET_BIT = IMPLICIT_BIT >> 1;
	const $Rep QNAN_REP = EXPONENT_MASK | QUIET_BIT;
	const $Rep INF_REP = EXPONENT_MASK;

	$Rep rep = bitcast(a, $Rep);
	$Rep abs = rep & ABS_MASK;
	int sign = rep & SIGN_BIT ? -1 : 1;
	int exponent = (int)((abs >> SIGNIFICANT_BITS) - EXPONENT_BIAS);
	$Rep significand = (abs & SIGNIFICANT_MASK) | IMPLICIT_BIT;
	if (sign == -1 || exponent < 0) return 0u128;
	if ((uint)exponent >= uint128.sizeof * 8) return ~0u128;
	if (exponent < SIGNIFICANT_BITS) return (uint128)significand >> (SIGNIFICANT_BITS - exponent);
	return (uint128)significand << (exponent - SIGNIFICANT_BITS);
}

macro fixint(a) @private
{
	var $Rep;
	$switch ($typeof(a))
		$case double:
			$Rep = ulong;
			const EXPONENT_BITS = 11;
			const SIGNIFICANT_BITS = 52;
		$case float:
			$Rep = uint;
			const EXPONENT_BITS = 8;
			const SIGNIFICANT_BITS = 23;
		$case float16:
			$Rep = ushort;
			const EXPONENT_BITS = 5;
			const SIGNIFICANT_BITS = 10;
		$case float128:
			$Rep = uint128;
			const EXPONENT_BITS = 15;
			const SIGNIFICANT_BITS = 112;
	$endswitch
	const $Rep MAX_EXPONENT = ($Rep)1 << EXPONENT_BITS - 1u;
	const $Rep EXPONENT_BIAS = MAX_EXPONENT >> 1u;
	const $Rep ONE_REP = EXPONENT_BIAS << SIGNIFICANT_BITS;
	const $Rep SIGN_BIT = ($Rep)1 << (EXPONENT_BITS + SIGNIFICANT_BITS);
	const $Rep ABS_MASK = SIGN_BIT - 1u;
	const $Rep IMPLICIT_BIT = ($Rep)1 << SIGNIFICANT_BITS;
	const $Rep SIGNIFICANT_MASK = IMPLICIT_BIT - 1u;
	const $Rep EXPONENT_MASK = ABS_MASK ^ SIGNIFICANT_MASK;
	const $Rep QUIET_BIT = IMPLICIT_BIT >> 1;
	const $Rep QNAN_REP = EXPONENT_MASK | QUIET_BIT;
	const $Rep INF_REP = EXPONENT_MASK;

	$Rep rep = bitcast(a, $Rep);
	$Rep abs = rep & ABS_MASK;
	int sign = rep & SIGN_BIT ? -1 : 1;
	int exponent = (int)((abs >> SIGNIFICANT_BITS) - EXPONENT_BIAS);
	$Rep significand = (abs & SIGNIFICANT_MASK) | IMPLICIT_BIT;
	if (exponent < 0) return 0;
	if ((uint)exponent >= uint128.sizeof * 8) return sign == 1 ? int128.max : int128.min;

	if (exponent < SIGNIFICANT_BITS) return sign * ((int128)significand >> (SIGNIFICANT_BITS - exponent));
	return sign * ((int128)significand << (exponent - SIGNIFICANT_BITS));
}
