var str:String = "";
for (var i:int = 1; i <= 5; i++) {
	for (var j:int = 1; j <= i; j++)
		str += "*";
	trace(str);
	str = "";
}
