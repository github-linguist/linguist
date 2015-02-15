set R to text returned of (display dialog "Enter number of rows:" default answer 2) as integer
set c to text returned of (display dialog "Enter number of columns:" default answer 2) as integer
set array to {}
repeat with i from 1 to R
	set temp to {}
	repeat with j from 1 to c
		set temp's end to 0
	end repeat
	set array's end to temp
end repeat

-- Address the first column of the first row:
set array's item 1's item 1 to -10

-- Negative index values can be used to address from the end:
set array's item -1's item -1 to 10

-- Access an item (row 2 column 1):
set x to array's item 2's item 1

return array

-- Destroy array (typically unnecessary since it'll automatically be destroyed once script ends).
set array to {}
