note
	description    : "Initial part, in order, of the sequence of Hamming numbers"
	math           : "[
			   Hamming numbers, also known as regular numbers and 5-smooth numbers, are natural integers
			   that have 2, 3 and 5 as their only prime factors.
		         ]"
	computer_arithmetic :
	                 "[
			   This version avoids integer overflow and stops at the last representable number in the sequence.
		         ]"
	output         : "[
    			   Per requirements of the RosettaCode example, execution will produce items of indexes 1 to 20 and 1691.
    			   The algorithm (procedure `hamming') is more general and will produce the first `n' Hamming numbers
    			   for any `n'.
    			  ]"
	source         : "This problem was posed in Edsger W. Dijkstra, A Discipline of Programming, Prentice Hall, 1978"
	date           : "8 August 2012"
	authors        : "Bertrand Meyer", "Emmanuel Stapf"
	revision       : "1.0"
	libraries      : "Relies on SORTED_TWO_WAY_LIST from EiffelBase"
	implementation : "[
			   Using SORTED_TWO_WAY_LIST provides an elegant illustration of how to implement
			   a lazy scheme in Eiffel through the use of object-oriented data structures.
			 ]"
	warning        : "[
			   The formatting (<lang>) specifications for Eiffel in RosettaCode are slightly obsolete:
			   `note' and other newer keywords not supported, red color for manifest strings.
			   This should be fixed soon.
		         ]"

class
	APPLICATION

create
	make

feature {NONE} -- Initialization

	make
			-- Print first 20 Hamming numbers, in order, and the 1691-st one.
		local
			Hammings: like hamming
				-- List of Hamming numbers, up to 1691-st one.
		do
			Hammings := hamming (1691)
			across 1 |..| 20 as i loop
				io.put_natural (Hammings.i_th (i.item)); io.put_string (" ")
			end
			io.put_new_line; io.put_natural (Hammings.i_th (1691)); io.put_new_line
		end

feature -- Basic operations

	hamming (n: INTEGER): ARRAYED_LIST [NATURAL]
			-- First `n' elements (in order) of the Hamming sequence,
			-- or as many of them as will not produce overflow.
		local
			sl: SORTED_TWO_WAY_LIST [NATURAL]
			overflow: BOOLEAN
			first, next: NATURAL
		do
			create Result.make (n); create sl.make
			sl.extend (1); sl.start
			across 1 |..| n as i invariant
				-- "The numbers output so far are the first `i' - 1 Hamming numbers, in order".
				-- "Result.first is the `i'-th Hamming number."
			until sl.is_empty loop
				first := sl.first; sl.start
				Result.extend (first); sl.remove
				across << 2, 3, 5 >> as multiplier loop
					next := multiplier.item * first
					overflow := overflow or next <= first
					if not overflow and then not sl.has (next) then sl.extend (next) end
				end
			end
		end
end
