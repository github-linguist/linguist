# GAP has built-in support for quaternions

A := QuaternionAlgebra(Rationals);
# <algebra-with-one of dimension 4 over Rationals>

b := BasisVectors(Basis(A));
# [ e, i, j, k ]

q := [1, 2, 3, 4]*b;
# e+(2)*i+(3)*j+(4)*k

# Conjugate
ComplexConjugate(q);
# e+(-2)*i+(-3)*j+(-4)*k

# Division
1/q;
# (1/30)*e+(-1/15)*i+(-1/10)*j+(-2/15)*k

# Computing norm may be difficult, since the result would be in a quadratic field.
# Sqrt exists in GAP, but it is quite unusual: see ?E in GAP documentation, and the following example
Sqrt(5/3);
# 1/3*E(60)^7+1/3*E(60)^11-1/3*E(60)^19-1/3*E(60)^23-1/3*E(60)^31+1/3*E(60)^43-1/3*E(60)^47+1/3*E(60)^59

# However, the square of the norm is easy to compute
q*ComplexConjugate(q);
# (30)*e

q1 := [2, 3, 4, 5]*b;
# (2)*e+(3)*i+(4)*j+(5)*k

q2 := [3, 4, 5, 6]*b;
# (3)*e+(4)*i+(5)*j+(6)*k

q1*q2 - q2*q1;
# (-2)*i+(4)*j+(-2)*k

# Can't add directly to a rational, one must make a quaternion of it
r := 5/3*b[1];
# (5/3)*e
r + q;
# (8/3)*e+(2)*i+(3)*j+(4)*k

# For multiplication, no problem (we are in an algebra over rationals !)
r*q;
# (5/3)*e+(10/3)*i+(5)*j+(20/3)*k
5/3*q;
# (5/3)*e+(10/3)*i+(5)*j+(20/3)*k

# Negative
-q;
(-1)*e+(-2)*i+(-3)*j+(-4)*k


# While quaternions are built-in, you can define an algebra in GAP by specifying it's multiplication table.
# See tutorial, p. 60, and reference of the functions used below.

# A multiplication table of dimension 4.

T := EmptySCTable(4, 0);
SetEntrySCTable(T, 1, 1, [1, 1]);
SetEntrySCTable(T, 1, 2, [1, 2]);
SetEntrySCTable(T, 1, 3, [1, 3]);
SetEntrySCTable(T, 1, 4, [1, 4]);
SetEntrySCTable(T, 2, 1, [1, 2]);
SetEntrySCTable(T, 2, 2, [-1, 1]);
SetEntrySCTable(T, 2, 3, [1, 4]);
SetEntrySCTable(T, 2, 4, [-1, 3]);
SetEntrySCTable(T, 3, 1, [1, 3]);
SetEntrySCTable(T, 3, 2, [-1, 4]);
SetEntrySCTable(T, 3, 3, [-1, 1]);
SetEntrySCTable(T, 3, 4, [1, 2]);
SetEntrySCTable(T, 4, 1, [1, 4]);
SetEntrySCTable(T, 4, 2, [1, 3]);
SetEntrySCTable(T, 4, 3, [-1, 2]);
SetEntrySCTable(T, 4, 4, [-1, 1]);

A := AlgebraByStructureConstants(Rationals, T, ["e", "i", "j", "k"]);
b := GeneratorsOfAlgebra(A);

IsAssociative(A);
# true

IsCommutative(A);
# false

# Then, like above

q := [1, 2, 3, 4]*b;
# e+(2)*i+(3)*j+(4)*k

# However, as is, GAP does not know division or conjugate on this algebra.
# QuaternionAlgebra is useful as well for extensions of rationals,
# and this one _has_ conjugate and division, as seen previously.

# Try this on Q[z] where z is the square root of 5 (in GAP it's ER(5))
F := FieldByGenerators([ER(5)]);
A := QuaternionAlgebra(F);
b := GeneratorsOfAlgebra(A);

q := [1, 2, 3, 4]*b;
# e+(2)*i+(3)*j+(4)*k

# Conjugate and division

ComplexConjugate(q);
# e+(-2)*i+(-3)*j+(-4)*k

1/q;
# (1/30)*e+(-1/15)*i+(-1/10)*j+(-2/15)*k
