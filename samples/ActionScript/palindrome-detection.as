function isPalindrome(str:String):Boolean
{
	for(var first:uint = 0, second:uint = str.length - 1; first < second; first++, second--)
		if(str.charAt(first) != str.charAt(second)) return false;
	return true;
}
