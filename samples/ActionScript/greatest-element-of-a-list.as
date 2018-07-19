function max(... args):Number
{
	var curMax:Number = -Infinity;
	for(var i:uint = 0; i < args.length; i++)
		curMax = Math.max(curMax, args[i]);
	return curMax;
}
