set n to 5
set k to 3

on calculateFactorial(val)
	set partial_factorial to 1 as integer
	repeat with i from 1 to val
		set factorial to i * partial_factorial
		set partial_factorial to factorial
	end repeat
	return factorial
end calculateFactorial

set n_factorial to calculateFactorial(n)
set k_factorial to calculateFactorial(k)
set n_minus_k_factorial to calculateFactorial(n - k)

return n_factorial / (n_minus_k_factorial) * 1 / (k_factorial) as integer
