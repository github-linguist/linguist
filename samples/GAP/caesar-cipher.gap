CaesarCipher := function(s, n)
	local r, c, i, lower, upper;
	lower := "abcdefghijklmnopqrstuvwxyz";
	upper := "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
	r := "";
	for c in s do
		i := Position(lower, c);
		if i <> fail then
			Add(r, lower[RemInt(i + n - 1, 26) + 1]);
		else
			i := Position(upper, c);
			if i <> fail then
				Add(r, upper[RemInt(i + n - 1, 26) + 1]);
			else
				Add(r, c);
			fi;
		fi;
	od;
	return r;
end;

CaesarCipher("IBM", 25);
# "HAL"

CaesarCipher("Vgg cphvi wzdibn vmz wjmi amzz viy zlpvg di ydbidot viy mdbcon.", 5);
# "All human beings are born free and equal in dignity and rights."
