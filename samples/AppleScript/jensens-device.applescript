set i to 0

on jsum(i, lo, hi, term)
	set {temp, i's contents} to {0, lo}
	repeat while i's contents ≤ hi
		set {temp, i's contents} to {temp + (term's f(i)), (i's contents) + 1}
	end repeat
	return temp
end jsum

script term_func
	on f(i)
		return 1 / i
	end f
end script

return jsum(a reference to i, 1, 100, term_func)
