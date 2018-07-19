IsDeranged := function(a, b)
	local i, n;
	for i in [1 .. Size(a)] do
		if a[i] = b[i] then
			return false;
		fi;
	od;
	return true;
end;

# This solution will find all deranged pairs of any length.
Deranged := function(name)
	local sol, ana, u, v;
	sol := [ ];
	ana := Anagrams(name);
	for u in ana do
		for v in Combinations(u, 2) do
			if IsDeranged(v[1], v[2]) then
				Add(sol, v);
			fi;
		od;
	od;
	return sol;
end;

# Now we find all deranged pairs of maximum length
a := Deranged("unixdict.txt");;
n := Maximum(List(a, x -> Size(x[1])));
Filtered(a, x -> Size(x[1]) = n);
# [ [ "excitation", "intoxicate" ] ]
