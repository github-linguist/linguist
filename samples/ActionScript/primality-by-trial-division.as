function isPrime(n:int):Boolean
{
	if(n < 2) return false;
	if(n == 2) return true;
	if((n & 1) == 0) return false;
	for(var i:int = 3; i <= Math.sqrt(n); i+= 2)
		if(n % i == 0) return false;
	return true;
}
