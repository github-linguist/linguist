set fibs to {}
set x to (text returned of (display dialog "What fibbonaci number do you want?" default answer "3"))
set x to x as integer
repeat with y from 1 to x
	if (y = 1 or y = 2) then
		copy 1 to the end of fibs
	else
		copy ((item (y - 1) of fibs) + (item (y - 2) of fibs)) to the end of fibs
	end if
end repeat
return item x of fibs
