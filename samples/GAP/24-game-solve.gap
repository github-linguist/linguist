# Solution in '''RPN'''
check := function(x, y, z)
	local r, c, s, i, j, k, a, b, p;
	i := 0;
	j := 0;
	k := 0;
	s := [ ];
	r := "";
	for c in z do
		if c = 'x' then
			i := i + 1;
			k := k + 1;
			s[k] := x[i];
			Append(r, String(x[i]));
		else
			j := j + 1;
			b := s[k];
			k := k - 1;
			a := s[k];
			p := y[j];
			r[Size(r) + 1] := p;
			if p = '+' then
				a := a + b;
			elif p = '-' then
				a := a - b;
			elif p = '*' then
				a := a * b;
			elif p = '/' then
				if b = 0 then
					continue;
				else
					a := a / b;
				fi;
			else
				return fail;
			fi;
			s[k] := a;
		fi;
	od;
	if s[1] = 24 then
		return r;
	else
		return fail;
	fi;
end;
	
Player24 := function(digits)
	local u, v, w, x, y, z, r;
	u := PermutationsList(digits);
	v := Tuples("+-*/", 3);
	w := ["xx*x*x*", "xx*xx**", "xxx**x*", "xxx*x**", "xxxx***"];
	for x in u do
		for y in v do
			for z in w do
				r := check(x, y, z);
				if r <> fail then
					return r;
				fi;
			od;
		od;
	od;
	return fail;
end;

Player24([1,2,7,7]);
# "77*1-2/"
Player24([9,8,7,6]);
# "68*97-/"
Player24([1,1,7,7]);
# fail

# Solutions with only one distinct digit are found only for 3, 4, 5, 6:
Player24([3,3,3,3]);
# "33*3*3-"
Player24([4,4,4,4]);
# "44*4+4+"
Player24([5,5,5,5]);
# "55*55/-"
Player24([6,6,6,6]);
# "66*66+-"

# A tricky one:
Player24([3,3,8,8]);
"8383/-/"
