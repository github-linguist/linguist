//Euclidean algorithm
function gcd(a:int,b:int):int
{
	var tmp:int;
	//Swap the numbers so a >= b
	if(a < b)
	{
		tmp = a;
		a = b;
		b = tmp;
	}
	//Find the gcd
	while(b != 0)
	{
		tmp = a % b;
		a = b;
		b = tmp;
	}
	return a;
}
