# Just multiplying a vector by itself yields the sum of squares (it's an inner product)
# It's necessary to check for the empty vector though
SumSq := function(v)
	if Size(v) = 0 then
		return 0;
	else
		return v*v;
	fi;
end;
