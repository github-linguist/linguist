set n to 12 -- Size of table.
repeat with x from 0 to n
	if x = 0 then set {table, x} to {{return}, -1}
	repeat with y from 0 to n
		if y's contents = 0 then
			if x > 0 then set row to {f(x)}
			if x = -1 then set {row, x} to {{f("x")}, 1}
		else
			if y ≥ x then set end of row to f(x * y)
			if y < x then set end of row to f("")
		end if
	end repeat
	set end of table to row & return
end repeat
return table as string

-- Handler/Function for formatting fixed width integer string.
on f(x)
	set text item delimiters to ""
	return (characters -4 thru -1 of ("    " & x)) as string
end f
