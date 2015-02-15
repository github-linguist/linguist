# Apply a function to a value, given variable names for both function and value
CheckEval := function(fun, val)
	local f, x;
	if IsBoundGlobal(fun) and IsBoundGlobal(val) then
		f := ValueGlobal(fun);
		x := ValueGlobal(val);
		return f(x);
	fi;
end;

bloop := -1;
CheckEval("AbsInt", "bloop");
# 1


# Sum of integer variables
GlobalIntegers := function()
	local s, x;
	s := 0;
	for name in SortedList(NamesGVars()) do
		if IsBoundGlobal(name) then
			x := ValueGlobal(name);
			if IsInt(x) then
				Print(name, " ", x, "\n");
				s := s + x;
			fi;
		fi;
	od;
	return s;
end;
