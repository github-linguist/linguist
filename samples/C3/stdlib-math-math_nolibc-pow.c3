// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/math_nolibc/pow.c3
// Permalink: https://github.com/c3lang/c3c/blob/4c1edfb/lib/std/math/math_nolibc/pow.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math::nolibc @if(env::NO_LIBC);

fn float powf_broken(float x, float f) @extern("powf") @weak @nostrip
{
	unreachable("'powf' not supported");
}

fn double pow_broken(double x, double y) @extern("pow") @weak @nostrip
{
	unreachable("'pow' not supported");
}
