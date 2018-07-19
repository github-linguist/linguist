expon := function(a, n, one, mul)
	local p;
	p := one;
	while n > 0 do
		if IsOddInt(n) then
			p := mul(a, p);
		fi;
		a := mul(a, a);
		n := QuoInt(n, 2);
	od;
	return p;
end;

expon(2, 10, 1, \*);
# 1024

# a more creative use of exponentiation
List([0 .. 31], n -> (1 - expon(0, n, 1, \-))/2);
# [ 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
#   1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1 ]
