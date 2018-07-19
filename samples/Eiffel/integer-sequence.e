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
			from
				number := 0
			until
				number = number.max_value
			loop
				print(number)
				print(", ")
				number := number + 1
			end
		end
	number:INTEGER_64
end
