a := [85, 88, 75, 66, 25, 29, 83, 39, 97];
b := [68, 41, 10, 49, 16, 65, 32, 92, 28, 98];

# Compute a decimal approximation of a rational
Approx := function(x, d)
	local neg, a, b, n, m, s;
	if x < 0 then
		x := -x;
		neg := true;
	else
		neg := false;
	fi;
	a := NumeratorRat(x);
	b := DenominatorRat(x);
	n := QuoInt(a, b);
	a := RemInt(a, b);
	m := 10^d;
	s := "";
	if neg then
		Append(s, "-");
	fi;
	Append(s, String(n));
	n := Size(s) + 1;
	Append(s, String(m + QuoInt(a*m, b)));
	s[n] := '.';
	return s;
end;

PermTest := function(a, b)
	local c, d, p, q, u, v, m, n, k, diff, all;
	p := Size(a);
	q := Size(b);
	v := Concatenation(a, b);
	n := p + q;
	m := Binomial(n, p);
	diff := Sum(a)/p - Sum(b)/q;
	all := [1 .. n];
	k := 0;
	for u in Combinations(all, p) do
		c := List(u, i -> v[i]);
		d := List(Difference(all, u), i -> v[i]);
		if Sum(c)/p - Sum(d)/q > diff then
			k := k + 1;
		fi;
	od;
	return [Approx((1 - k/m)*100, 3), Approx(k/m*100, 3)];
end;

# in order, % less or greater than original diff
PermTest(a, b);
[ "87.197", "12.802" ]
