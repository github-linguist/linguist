DotProduct := function(u, v)
	return u*v;
end;

CrossProduct := function(u, v)
	return [
		u[2]*v[3] - u[3]*v[2],
		u[3]*v[1] - u[1]*v[3],
		u[1]*v[2] - u[2]*v[1] ];
end;

ScalarTripleProduct := function(u, v, w)
	return DotProduct(u, CrossProduct(v, w));
end;

VectorTripleProduct := function(u, v, w)
	return CrossProduct(u, CrossProduct(v, w));
end;

a := [3, 4, 5];
b := [4, 3, 5];
c := [-5, -12, -13];

DotProduct(a, b);
# 49

CrossProduct(a, b);
# [ 5, 5, -7 ]

ScalarTripleProduct(a, b, c);
# 6

# Another way to get it
Determinant([a, b, c]);
# 6

VectorTripleProduct(a, b, c);
# [ -267, 204, -3 ]
