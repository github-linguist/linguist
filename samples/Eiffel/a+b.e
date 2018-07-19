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
			print(argument(1).to_integer +	argument(2).to_integer)
		end
end
