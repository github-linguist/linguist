function isValid(numString:String):Boolean
{
	var isOdd:Boolean = true;
	var oddSum:uint = 0;
	var evenSum:uint = 0;
	for(var i:int = numString.length - 1; i >= 0; i--)
	{
		var digit:uint = uint(numString.charAt(i))
		if(isOdd) oddSum += digit;
		else evenSum += digit/5 + (2*digit) % 10;
		isOdd = !isOdd;
	}
	if((oddSum + evenSum) % 10 == 0) return true;
	return false;
}

trace(isValid("49927398716"));
trace(isValid("49927398717"));
trace(isValid("1234567812345678"));
trace(isValid("1234567812345670"));
