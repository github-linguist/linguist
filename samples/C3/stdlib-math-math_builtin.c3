// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/math_builtin.c3
// Permalink: https://github.com/c3lang/c3c/blob/a0a5c94/lib/std/math/math_builtin.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math;

fn float __roundevenf(float f) @extern("roundevenf") @weak @nostrip
{
	// Slow implementation
	return round(f / 2) * 2;
}

fn double __roundeven(double d) @extern("roundeven") @weak @nostrip
{
	// Slow implementation
	return round(d / 2) * 2;
}
