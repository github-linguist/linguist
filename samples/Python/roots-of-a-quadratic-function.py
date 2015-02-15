>>> def quad_discriminating_roots(a,b,c, entier = 1e-5):
	discriminant = b*b - 4*a*c
	a,b,c,d =complex(a), complex(b), complex(c), complex(discriminant)
	root1 = (-b + d**0.5)/2./a
	root2 = (-b - d**0.5)/2./a
	if abs(discriminant) < entier:
		return "real and equal", abs(root1), abs(root1)
	if discriminant > 0:
		return "real", root1.real, root2.real
	return "complex", root1, root2

>>> for coeffs in ((3, 4, 4/3.), (3, 2, -1), (3, 2, 1), (1.0, -10.0E5, 1.0)):
	print "Roots of: %gX^2 %+gX %+g are" % coeffs
	print "  %s: %s, %s" % quad_discriminating_roots(*coeffs)

	
Roots of: 3X^2 +4X +1.33333 are
  real and equal: 0.666666666667, 0.666666666667
Roots of: 3X^2 +2X -1 are
  real: 0.333333333333, -1.0
Roots of: 3X^2 +2X +1 are
  complex: (-0.333333333333+0.471404520791j), (-0.333333333333-0.471404520791j)
Roots of: 1X^2 -1e+06X +1 are
  real: 999999.999999, 1.00000761449e-06
>>>
