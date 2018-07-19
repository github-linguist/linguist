class
	APPLICATION

create
	make

feature -- Initialization

	make
		local
			a: ARRAY [INTEGER]
		do
			a := <<1, -2, 3>>
			print ("%NSquare sum of <<1, 2, 3>>: " + sum_of_square (a).out)

			a := <<>>
			print ("%NSquare sum of <<>>: " + sum_of_square (a).out)
		end

feature -- Access

	sum_of_square (a: ITERABLE [INTEGER]): NATURAL
			-- sum of square of each items
		do
			Result := 0
			across a as it loop
				Result := Result + (it.item * it.item).as_natural_32
			end
		end

end
