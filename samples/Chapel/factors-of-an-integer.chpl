iter factors(n) {
	for i in 1..floor(sqrt(n)):int {
		if n % i == 0 then {
			yield i;
			yield n / i;
		}
	}
}
