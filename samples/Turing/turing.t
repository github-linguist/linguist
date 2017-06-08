% Accepts a number and calculates its factorial

function factorial (n: int) : real
	if n = 0 then
		result 1
	else
		result n * factorial (n - 1)
   end if
end factorial

var n: int
loop
	put "Please input an integer: " ..
	get n
	exit when n >= 0
	put "Input must be a non-negative integer."
end loop

put "The factorial of ", n, " is ", factorial (n)