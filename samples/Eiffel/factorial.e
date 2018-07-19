note
	description: "recursive and iterative factorial example of a positive integer."

class
	FACTORIAL_EXAMPLE

create
	make

feature -- Initialization

	make
		local
			n: NATURAL
		do
			n := 5
			print ("%NFactorial of " + n.out + " = ")
			print (recursive_factorial (n))
		end

feature -- Access

	recursive_factorial (n: NATURAL): NATURAL
			-- factorial of 'n'
		do
			if n = 0 then
				Result := 1
			else
				Result := n * recursive_factorial (n - 1)
			end
		end

	iterative_factorial (n: NATURAL): NATURAL
			-- factorial of 'n'
		local
			v: like n
		do
			from
				Result := 1
				v := n
			until
				v <= 1
			loop
				Result := Result * v
				v := v - 1
			end
		end

end
