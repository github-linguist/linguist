function partialSum(n:uint):Number
{
	var sum:Number = 0;
	for(var i:uint = 1; i <= n; i++)
		sum += 1/(i*i);
	return sum;
}
trace(partialSum(1000));
