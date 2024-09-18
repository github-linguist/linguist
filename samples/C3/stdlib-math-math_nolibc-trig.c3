// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/math_nolibc/trig.c3
// Permalink: https://github.com/c3lang/c3c/blob/4c1edfb/lib/std/math/math_nolibc/trig.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math::nolibc @if(env::NO_LIBC);

fn double sincos_broken(double x) @extern("sincos") @weak @nostrip
{
	unreachable("'sinccos' not supported");
}


