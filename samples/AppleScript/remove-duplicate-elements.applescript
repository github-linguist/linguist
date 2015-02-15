unique({1, 2, 3, "a", "b", "c", 2, 3, 4, "b", "c", "d"})

on unique(x)
	set R to {}
	repeat with i in x
		if i is not in R then set end of R to i's contents
	end repeat
	return R
end unique
