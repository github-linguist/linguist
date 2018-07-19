function reverseString(string:String):String
{
	var reversed:String = new String();
	for(var i:int = string.length -1; i >= 0; i--)
		reversed += string.charAt(i);
	return reversed;
}

function reverseStringCQAlternative(string:String):String
{
	return string.split('').reverse().join('');
}
