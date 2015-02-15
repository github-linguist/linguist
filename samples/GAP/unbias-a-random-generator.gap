RandNGen := function(n)
	local v, rand;
	v := [1 .. n - 1]*0;
	Add(v, 1);
	rand := function()
		return Random(v);
	end;
	return rand;
end;

UnbiasedGen := function(rand)
	local unbiased;
	unbiased := function()
		local a, b;
		while true do
			a := rand();
			b := rand();
			if a <> b then
				break;
			fi;
		od;
		return a;
	end;
	return unbiased;
end;

range := [2 .. 6];
v := List(range, RandNGen);
w := List(v, UnbiasedGen);
apply := gen -> Sum([1 .. 1000000], n -> gen());

# Some tests (2 is added as a witness, since in this case RandN is already unbiased)
PrintArray(TransposedMat([range, List(v, apply), List(w, apply)]));
# [ [       2,  499991,  499041 ],
#   [       3,  333310,  500044 ],
#   [       4,  249851,  500663 ],
#   [       5,  200532,  500448 ],
#   [       6,  166746,  499859 ] ]
