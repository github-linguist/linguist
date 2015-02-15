Bottles := function(n)
	local line, i, j, u;
	line := function(n)
		s := String(n);
		if n < 2 then
			return Concatenation(String(n), " bottle of beer");
		else
			return Concatenation(String(n), " bottles of beer");
		fi;
	end;
	for i in [1 .. n] do
		j := n - i + 1;
		u := line(j);
		Display(Concatenation(u, " on the wall"));
		Display(u);
		Display("Take one down, pass it around");
		Display(Concatenation(line(j - 1), " on the wall"));
		if i <> n then
			Display("");
		fi;
	od;
end;
