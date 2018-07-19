class
	APPLICATION

inherit
	ARGUMENTS

create
	make

feature {NONE} -- Initialization
	make
			-- Run application.
		do
			-- initialize the array, index starts at 1 (not zero) and prefill everything with the letter z
			create my_static_array.make_filled ("z", 1, 50)

			my_static_array.put ("a", 1)
			my_static_array.put ("b", 2)
			my_static_array [3] := "c"

			-- access to array fields
			print (my_static_array.at(1) + "%N")
			print (my_static_array.at(2) + "%N")
			print (my_static_array [3] + "%N")

			-- in Eiffel static arrays can be resized in three ways
			my_static_array.force ("c", 51) -- forces 'c' in position 51 and resizes the array to that size (now 51 places)
			my_static_array.automatic_grow -- adds 50% more indices (having now 76 places)
			my_static_array.grow (100) -- resizes the array to 100 places
		end

	my_static_array: ARRAY [STRING]
end
