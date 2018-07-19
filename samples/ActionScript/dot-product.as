function dotProduct(v1:Vector.<Number>, v2:Vector.<Number>):Number
{
	if(v1.length != v2.length) return NaN;
	var sum:Number = 0;
	for(var i:uint = 0; i < v1.length; i++)
		sum += v1[i]*v2[i];
	return sum;
}
trace(dotProduct(Vector.<Number>([1,3,-5]),Vector.<Number>([4,-2,-1])));
