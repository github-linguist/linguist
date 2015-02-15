proc fizzbuzz(n) {
	for i in 1..n do
		if i % 15 == 0 then
			writeln("FizzBuzz");
		else if i % 5 == 0 then
			writeln("Buzz");
		else if i % 3 == 0 then
			writeln("Fizz");
		else
			writeln(i);
}

fizzbuzz(100);
