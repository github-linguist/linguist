note
	description : "{
2 Examples are given.
The first example uses the standard library's FORMAT_DOUBLE class.
The second example uses the AEL_PRINTF class from the freely available
Amalasoft Eiffel Library (AEL).

See additional comments in the code.
}"

class APPLICATION

inherit
	AEL_PRINTF -- Optional, see below

create
	make

feature {NONE} -- Initialization

	make
			-- Run application.
		do
			print_formatted_std (7.125)
			print_formatted_ael (7.125)
		end

	--|--------------------------------------------------------------

	print_formatted_std (v: REAL_64)
			-- Print the value 'v' as a zero-padded string in a fixed
			-- overall width of 9 places and, with a precision of
			-- to 3 places to the right of the decimal point.
			-- Use the FORMAT_DOUBLE class from the standard library
		local
			fmt: FORMAT_DOUBLE
		do
			create fmt.make (9, 3)
			fmt.zero_fill
			print (fmt.formatted (v) + "%N")
		end

	--|--------------------------------------------------------------

	print_formatted_ael (v: REAL_64)
			-- Print the value 'v' as a zero-padded string in a fixed
			-- overall width of 9 places and, with a precision of
			-- to 3 places to the right of the decimal point.
			-- Use the AEL_PRINTF class from the Amalasoft Eiffel Library
			-- freely available from www.amalasoft.com
		do
			-- printf accepts a format string and an argument list
			-- The argument list is a container (often a manifest
			-- array) of values corresponding to the type of the format
			-- specified in the format string argument.
			-- When only one argument is needed, then there is also the
			-- option to use just the value, without the container.
			-- In this example, the line would be:
			--   printf ("%%09.3f%N", v)
			-- The more deliberate form is used in the actual example,
			-- as it is more representative of common usage, when there
			-- are multiple value arguments.

			printf ("%%09.3f%N", << v >>)
		end

end
