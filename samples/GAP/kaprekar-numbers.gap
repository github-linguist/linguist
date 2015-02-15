IsKaprekar := function(n)
	local a, b, p, q;
	if n = 1 then
		return true;
	fi;
	q := n*n;
	p := 10;
	while p < q do
		a := RemInt(q, p);
		b := QuoInt(q, p);
		if a > 0 and a + b = n then
			return true;
		fi;
		p := p*10;
	od;
	return false;
end;

Filtered([1 .. 10000], IsKaprekar);
# [ 1, 9, 45, 55, 99, 297, 703, 999, 2223, 2728, 4879, 4950, 5050, 5292, 7272,
#   7777, 9999 ]

Size(last);
# 17

Filtered([1 .. 1000000], IsKaprekar);
# [ 1, 9, 45, 55, 99, 297, 703, 999, 2223, 2728, 4879, 4950, 5050, 5292, 7272,
#   7777, 9999, 17344, 22222, 38962, 77778, 82656, 95121, 99999, 142857,
#   148149, 181819, 187110, 208495, 318682, 329967, 351352, 356643, 390313,
#   461539, 466830, 499500, 500500, 533170, 538461, 609687, 627615, 643357,
#   648648, 670033, 681318, 791505, 812890, 818181, 851851, 857143, 961038,
#   994708, 999999 ]

Size(last);
# 54


IsKaprekarAndHow := function(n, base)
	local a, b, p, q;
	if n = 1 then
		return true;
	fi;
	q := n*n;
	p := base;
	while p < q do
		a := RemInt(q, p);
		b := QuoInt(q, p);
		if a > 0 and a + b = n then
			return [a, b];
		fi;
		p := p*base;
	od;
	return false;
end;

IntegerToBaseRep := function(n, base)
	local s, digit;
	if base > 36 then
		return fail;
	elif n = 0 then
		return "0";
	else
		s := "";
		digit := "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
		while n <> 0 do
			Add(s, digit[RemInt(n, base) + 1]);
			n := QuoInt(n, base);
		od;
		return Reversed(s);
	fi;
end;

PrintIfKaprekar := function(n, base)
	local v;
	v := IsKaprekarAndHow(n, base);
	if IsList(v) then
		Print(n, "(10) or in base ", base, ", ",
			IntegerToBaseRep(n, base), "^2 = ",
			IntegerToBaseRep(n^2, base), " and ",
			IntegerToBaseRep(v[2], base), " + ",
			IntegerToBaseRep(v[1], base), " = ",
			IntegerToBaseRep(n, base), "\n");
	fi;
	return fail;
end;

# In base 17...
Perform([1 .. 1000000], n -> PrintIfKaprekar(n, 17));
# 16(10) or in base 17, G^2 = F1 and F + 1 = G
# 64(10) or in base 17, 3D^2 = E2G and E + 2G = 3D
# 225(10) or in base 17, D4^2 = A52G and A5 + 2G = D4
# 288(10) or in base 17, GG^2 = GF01 and GF + 1 = GG
# 1536(10) or in base 17, 556^2 = 1B43B2 and 1B4 + 3B2 = 556
# 3377(10) or in base 17, BBB^2 = 8093B2 and 809 + 3B2 = BBB
# 4912(10) or in base 17, GGG^2 = GGF001 and GGF + 1 = GGG
# 7425(10) or in base 17, 18BD^2 = 24E166G and 24E + 166G = 18BD
# 9280(10) or in base 17, 1F1F^2 = 39B1B94 and 39B + 1B94 = 1F1F
# 16705(10) or in base 17, 36DB^2 = B992C42 and B99 + 2C42 = 36DB
# 20736(10) or in base 17, 43CD^2 = 10DE32FG and 10DE + 32FG = 43CD
# 30016(10) or in base 17, 61EB^2 = 23593F92 and 2359 + 3F92 = 61EB
# 36801(10) or in base 17, 785D^2 = 351E433G and 351E + 433G = 785D
# 37440(10) or in base 17, 7A96^2 = 37144382 and 3714 + 4382 = 7A96
# 46081(10) or in base 17, 967B^2 = 52G94382 and 52G9 + 4382 = 967B
# 46720(10) or in base 17, 98B4^2 = 5575433G and 5575 + 433G = 98B4
# 53505(10) or in base 17, AF26^2 = 6GA43F92 and 6GA4 + 3F92 = AF26
# 62785(10) or in base 17, CD44^2 = 9A5532FG and 9A55 + 32FG = CD44
# 66816(10) or in base 17, DA36^2 = AEG42C42 and AEG4 + 2C42 = DA36
# 74241(10) or in base 17, F1F2^2 = D75F1B94 and D75F + 1B94 = F1F2
# 76096(10) or in base 17, F854^2 = E1F5166G and E1F5 + 166G = F854
# 83520(10) or in base 17, GGGG^2 = GGGF0001 and GGGF + 1 = GGGG
# 266224(10) or in base 17, 33334^2 = A2C52A07G and A2C5 + 2A07G = 33334
