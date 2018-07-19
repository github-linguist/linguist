//Return the code corresponding to a given character.
//ActionScript does not have a character type, so 1-digit strings
//are used instead
function toSEDOLCode(char:String):uint {
	//Make sure only a single character was sent.
	if(char.length != 1)
		throw new Error("toSEDOL expected string length of 1, got " + char.length);
	//Character is uppercase
	if (char >= "A" && char <= "Z") {
		return SEDOL.charCodeAt() + 10 - "A".charCodeAt();
	}
	//Character is numeric
	else if (char >= "0" && char <= "9"){
		return uint(char);
	}
	//Error: character is neither numeric nor uppercase
	else{
		throw new Error("toSEDOLCode expected numeric or uppercase character, recieved " + char);
	}
}
//Calculate the weighted sum for the SEDOL.
function toSum(str:String):uint {
	if(str.length != 6)
		throw new Error("toSum expected length 6, recieved " + str.length);
	var sum:uint=0;
	for (var i:uint = 0; i < str.length; i++)
		sum+=toSEDOLCode(str.charAt(i))*[1,3,1,7,3,9][i];
	return sum;
}
//Calculate the check digit from the weighted sum.
function toCheck(num:int):uint
{
	return (10 -(num % 10)) % 10;
}
//Print the SEDOL with the check digit added.
function printWithCheck(SEDOL:String):void
{
	trace(SEDOL + toCheck(toSum(SEDOL)));
}
printWithCheck("710889");
printWithCheck("B0YBKJ");
printWithCheck("406566");
printWithCheck("B0YBLH");
printWithCheck("228276");
printWithCheck("B0YBKL");
printWithCheck("557910");
printWithCheck("B0YBKR");
printWithCheck("585284");
printWithCheck("B0YBKT");
printWithCheck("B00030");
