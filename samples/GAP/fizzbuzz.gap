FizzBuzz := function()
	local i;
	for i in [1 .. 100] do
		if RemInt(i, 15) = 0 then
			Print("FizzBuzz\n");
		elif RemInt(i, 3) = 0 then
			Print("Fizz\n");
		elif RemInt(i, 5) = 0 then
			Print("Buzz\n");
		else
			Print(i, "\n");
		fi;
	od;
end;
