# Here we use generators : the given formula doesn't need one, but the alternate
# non-squares function is better done with a generator.

# The formula is implemented with exact floor(sqrt(n)), so we use
# a trick: multiply by 100 to get the first decimal digit of the
# square root of n, then add 5 (that's 1/2 multiplied by 10).
# Then just divide by 10 to get floor(1/2 + sqrt(n)) exactly.
# It looks weird, but unlike floating point, it will do the job
# for any n.
NonSquaresGen := function()
	local ns, n;
	n := 0;
	ns := function()
		n := n + 1;
		return n + QuoInt(5 + RootInt(100*n), 10);
	end;
	return ns;
end;

NonSquaresAlt := function()
	local ns, n, q, k;
	n := 1;
	q := 4;
	k := 3;
	ns := function()
		n := n + 1;
		if n = q then
			n := n + 1;
			k := k + 2;
			q := q + k;
		fi;
		return n;
	end;
	return ns;
end;

gen := NonSquaresGen();
List([1 .. 22] i -> gen());
# [ 2, 3, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 17, 18, 19, 20, 21, 22, 23, 24, 26, 27 ]

a := NonSquaresGen();
b := NonSquaresAlt();

ForAll([1 .. 1000000], i -> a() = b());
# true
