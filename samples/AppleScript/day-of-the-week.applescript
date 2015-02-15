set ChristmasSundays to {}
set Christmas to (current date)
set month of Christmas to December
set day of Christmas to 25
repeat with year from 2008 to 2121
	set year of Christmas to year
	if weekday of Christmas is Sunday then set end of ChristmasSundays to year
end repeat
ChristmasSundays
