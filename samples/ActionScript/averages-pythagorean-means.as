function arithmeticMean(v:Vector.<Number>):Number
{
	var sum:Number = 0;
	for(var i: uint = 0; i < v.length; i++)
		sum += v[i];
	return sum/v.length;
}
function geometricMean(v:Vector.<Number>):Number
{
	var product:Number = 1;
	for(var i: uint = 0; i < v.length; i++)
		product *= v[i];
	return Math.pow(product, 1/v.length);
}
function harmonicMean(v:Vector.<Number>):Number
{
	var sum:Number = 0;
	for(var i: uint = 0; i < v.length; i++)
		sum += 1/v[i];
	return v.length/sum;
}
var list:Vector.<Number> = Vector.<Number>([1,2,3,4,5,6,7,8,9,10]);
trace("Arithmetic: ", arithmeticMean(list));
trace("Geometric: ", geometricMean(list));
trace("Harmonic: ", harmonicMean(list));
