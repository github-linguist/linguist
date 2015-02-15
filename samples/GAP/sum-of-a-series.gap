# We will compute the sum exactly

# Computing an approximation of a rationnal (giving a string)
# Value is truncated toward zero
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

a := Sum([1 .. 1000], n -> 1/n^2);;
Approx(a, 10);
"1.6439345666"
# and pi^2/6 is 1.6449340668, truncated to ten digits
