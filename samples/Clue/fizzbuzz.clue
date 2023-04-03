local fn fizzbuzz(number) {
	local string = ""
	if number % 3 == 0 {
		string ..= "fizz"
	}
	if number % 5 == 0 {
		string ..= "buzz"
	}
	if string != "" {
		print(string .. ": " .. number)
	}
}

for i = 1, 100 {
	fizzbuzz(i)
}