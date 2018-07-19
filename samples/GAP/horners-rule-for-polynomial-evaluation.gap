# The idiomatic way to compute with polynomials

x := Indeterminate(Rationals, "x");

# This is a value in a polynomial ring, not a function
p := 6*x^3 - 4*x^2 + 7*x - 19;

Value(p, 3);
# 128

u := CoefficientsOfUnivariatePolynomial(p);
# [ -19, 7, -4, 6 ]

# One may also create the polynomial from coefficients
q := UnivariatePolynomial(Rationals, [-19, 7, -4, 6], x);
# 6*x^3-4*x^2+7*x-19

p = q;
# true

# Now a Horner implementation
Horner := function(coef, x)
	local v, c;
	v := 0;
	for c in Reversed(coef) do
		v := x*v + c;
	od;
	return v;
end;

Horner(u, 3);
# 128
