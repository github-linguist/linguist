note
	description    : "Prints pascal's triangle"
	output         : "[
    			   Per requirements of the RosettaCode example, execution will print the first n rows of pascal's triangle
    			  ]"
	date           : "19 December 2013"
	authors        : "Sandro Meier", "Roman Brunner"
	revision       : "1.0"
	libraries      : "Relies on HASH_TABLE from EIFFEL_BASE library"
	implementation : "[
			   Recursive implementation to calculate the n'th row.
			 ]"
	warning        : "[
				Will not work for large n's (INTEGER_32)
		         ]"

class
	APPLICATION

inherit
	ARGUMENTS

create
	make

feature {NONE} -- Initialization

	make
		local
			n:INTEGER
		do
			create {HASH_TABLE[ARRAY[INTEGER],INTEGER]}pascal_lines.make (n) --create the hash_table object
			io.new_line
			n:=25
			draw(n)
		end
feature
	line(n:INTEGER):ARRAY[INTEGER]
		--Calculates the n'th line
	local
		upper_line:ARRAY[INTEGER]
		i:INTEGER
	do
		if	n=1 then	--trivial case first line
			create Result.make_filled (0, 1, n+2)
			Result.put (0, 1)
			Result.put (1, 2)
			Result.put (0, 3)
		elseif pascal_lines.has (n) then	--checks if the result was already calculated
			Result := pascal_lines.at (n)
		else	--calculates the n'th line recursively
			create Result.make_filled(0,1,n+2) --for caluclation purposes add a 0 at the beginning of each line
			Result.put (0, 1)
			upper_line:=line(n-1)
			from
				i:=1
			until
				i>upper_line.count-1
			loop
				Result.put(upper_line[i]+upper_line[i+1],i+1)
				i:=i+1
			end
			Result.put (0, n+2)	--for caluclation purposes add a 0 at the end of each line
			pascal_lines.put (Result, n)
		end
	end

	draw(n:INTEGER)
		--draw n lines of pascal's triangle
	local
		space_string:STRING
		width, i:INTEGER

	do
		space_string:=" "		--question of design: add space_string at the beginning of each line
		width:=line(n).count
		space_string.multiply (width)
		from
			i:=1
		until
			i>n
		loop
			space_string.remove_tail (1)
			io.put_string (space_string)
			across line(i) as c
			loop
				if
					c.item/=0
				then
					io.put_string (c.item.out+" ")
				end
			end
			io.new_line
			i:=i+1
		end
	end

feature --Access
	pascal_lines:HASH_TABLE[ARRAY[INTEGER],INTEGER]
		--Contains all already calculated lines
end
