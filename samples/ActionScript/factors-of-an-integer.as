function factor(n:uint):Vector.<uint>
{
	var factors:Vector.<uint> = new Vector.<uint>();
	for(var i:uint = 1; i <= n; i++)
		if(n % i == 0)factors.push(i);
	return factors;
}
