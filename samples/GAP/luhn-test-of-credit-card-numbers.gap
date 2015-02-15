IsLuhn := function(n)
	local c, d, i, j, r;
	d := "0123456789";
	j := 1;
	r := 0;
	for c in Reversed(String(n)) do
		i := Position(d, c);
		if i = fail then
			continue;
		fi;
		i := j*(i - 1);
		r := r + QuoInt(i, 10) + RemInt(i, 10);
		j := 3 - j;
	od;
	return RemInt(r, 10) = 0;
end;

List([49927398716, 49927398717, 1234567812345678, 1234567812345670], IsLuhn);
# [ true, false, false, true ]

# Will also work on strings, and will skip non-digits
IsLuhn("4-992-739-871-6");
# true
