class
	APPLICATION

create
	make

feature

	fibonacci (n: INTEGER): INTEGER
		require
			non_negative: n >= 0
		local
			i, n2, n1, tmp: INTEGER
		do
			n2 := 0
			n1 := 1
			from
				i := 1
			until
				i >= n
			loop
				tmp := n1
				n1 := n2 + n1
				n2 := tmp
				i := i + 1
			end
			Result := n1
			if n = 0 then
				Result := 0
			end
		end

feature {NONE} -- Initialization

	make
			-- Run application.
		do
			print (fibonacci (0))
			print (" ")
			print (fibonacci (1))
			print (" ")
			print (fibonacci (2))
			print (" ")
			print (fibonacci (3))
			print (" ")
			print (fibonacci (4))
			print ("%N")
		end

end
