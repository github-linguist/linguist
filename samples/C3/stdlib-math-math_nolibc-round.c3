// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/math_nolibc/round.c3
// Permalink: https://github.com/c3lang/c3c/blob/4c1edfb/lib/std/math/math_nolibc/round.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math::nolibc @if(env::NO_LIBC);

fn double _round(double x) @extern("round") @weak @nostrip
{
	ulong u = bitcast(x, ulong);
	int e = (int)((u >> 52) & 0x7ff);
	if (e >= 0x3ff + 52) return x;
	if (u >> 63) x = -x;
	if (e < 0x3ff - 1)
	{
		/* raise inexact if x!=0 */
		force_eval_add(x, TOINT);
		return 0 * x;
	}
	double y = (x + TOINT) - TOINT - x;
	switch
	{
		case y > 0.5:
			y = y + x - 1;
		case y <= -0.5:
			y = y + x + 1;
		default:
			y = y + x;
	}
	if (u >> 63) y = -y;
	return y;
}

fn float _roundf(float x) @extern("roundf") @weak @nostrip
{
	uint u = bitcast(x, uint);
	int e = (u >> 23) & 0xff;
	if (e >= 0x7f + 23) return x;
	if (u >> 31) x = -x;
	if (e < 0x7f - 1)
	{
		force_eval_add(x, TOINTF);
		return 0 * x;
	}
	float y = (x + TOINTF) - TOINTF - x;
	switch
	{
		case y > 0.5f:
			y = y + x - 1;
		case y <= -0.5f:
			y = y + x + 1;
		default:
			y = y + x;
	}
	if (u >> 31) y = -y;
	return y;
}

