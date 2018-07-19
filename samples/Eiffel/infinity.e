class
	APPLICATION
inherit
	ARGUMENTS
create
	make
feature {NONE} -- Initialization
	number:REAL_64
	make
			-- Run application.
		do
			number := 2^2000
			print(number)
			print("%N")
			print(number.is_positive_infinity)
			print("%N")
		end
end
