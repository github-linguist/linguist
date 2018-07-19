function sumOfSquares(n:uint)
{
	var sum:uint = 0;
	while(n != 0)
	{
		sum += (n%10)*(n%10);
		n /= 10;
	}
	return sum;
}
function isInArray(n:uint, array:Array)
{
	for(var k = 0; k < array.length; k++)
		if(n == array[k]) return true;
	return false;
}
function isHappy(n)
{
	var sequence:Array = new Array();
	while(n != 1)
	{
		sequence.push(n);
		n = sumOfSquares(n);
		if(isInArray(n,sequence))return false;
	}
	return true;
}
function printHappy()
{
	var numbersLeft:uint = 8;
	var numberToTest:uint = 1;
	while(numbersLeft != 0)
	{
		if(isHappy(numberToTest))
		{
			trace(numberToTest);
			numbersLeft--;
		}
		numberToTest++;
	}
}
printHappy();
