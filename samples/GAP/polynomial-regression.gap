PolynomialRegression := function(x, y, n)
	local a;
	a := List([0 .. n], i -> List(x, s -> s^i));
	return TransposedMat((a * TransposedMat(a))^-1 * a * TransposedMat([y]))[1];
end;

x := [0,  1,  2,  3,  4,  5,  6,   7,   8,   9,   10];
y := [1,  6,  17, 34, 57, 86, 121, 162, 209, 262, 321];

# Return coefficients in ascending degree order
PolynomialRegression(x, y, 2);
# [ 1, 2, 3 ]
