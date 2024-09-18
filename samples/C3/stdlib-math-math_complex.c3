// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/math_complex.c3
// Permalink: https://github.com/c3lang/c3c/blob/e293c43/lib/std/math/math_complex.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math::complex(<Real>);

union Complex
{
	struct
	{
		Real r, c;
	}
	Real[<2>] v;
}


const Complex IDENTITY = { 1, 0 };
macro Complex Complex.add(self, Complex b) => Complex { .v = self.v + b.v };
macro Complex Complex.add_each(self, Real b) => Complex { .v = self.v + b };
macro Complex Complex.sub(self, Complex b) => Complex { .v = self.v - b.v };
macro Complex Complex.sub_each(self, Real b) => Complex { .v = self.v - b };
macro Complex Complex.scale(self, Real s) => Complex { .v = self.v * s };
macro Complex Complex.mul(self, Complex b) => { self.r * b.r - self.c * b.c, self.r * b.c + b.r * self.c };
macro Complex Complex.div(self, Complex b)
{
	Real div = b.v.dot(b.v);
	return Complex{ (self.r * b.r + self.c * b.c) / div, (self.c * b.r - self.r * b.c) / div };
}
