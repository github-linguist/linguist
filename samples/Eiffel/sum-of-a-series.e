note
	description: "Compute the n-th term of a series"

class
	SUM_OF_SERIES_EXAMPLE

inherit
	MATH_CONST

create
	make

feature -- Initialization

	make
		local
			approximated, known: REAL_64
		do
			known := Pi^2 / 6

			approximated := sum_until (agent g, 1001)
			print ("%Nzeta function exact value: %N")
			print (known)
			print ("%Nzeta function approximated value: %N")
			print (approximated)
		end

feature -- Access

	g (k: INTEGER): REAL_64
			-- 'k'-th term of the serie
		require
			k_positive: k > 0
		do
			Result := 1 / (k * k)
		end
	
	sum_until (s: FUNCTION [ANY, TUPLE [INTEGER], REAL_64]; n: INTEGER): REAL_64
			-- sum of the 'n' first terms of 's'
		require
			n_positive: n > 0
			one_parameter: s.open_count = 1
		do
			Result := 0
			across 1 |..| n as it loop
				Result := Result + s.item ([it.item])
			end
		end

end
