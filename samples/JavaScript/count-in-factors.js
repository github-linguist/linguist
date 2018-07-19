for(i = 1; i <= 10; i++)
    console.log(i + " : " + factor(i).join(" x "));

function factor(n) {
    var factors = [];
    if (n == 1) return [1];
    for(p = 2; p <= n; ) {
	if((n % p) == 0) {
	    factors[factors.length] = p;
	    n /= p;
	}
	else p++;
    }
    return factors;
}
