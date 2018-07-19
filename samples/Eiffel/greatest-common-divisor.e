class
	APPLICATION

create
	make

feature -- Implementation

	gcd (x: INTEGER y: INTEGER): INTEGER
		do
			if y = 0 then
				Result := x
			else
				Result := gcd (y, x \\ y);
			end
		end

feature {NONE} -- Initialization

	make
			-- Run application.
		do
			print (gcd (15, 10))
			print ("%N")
		end

end
