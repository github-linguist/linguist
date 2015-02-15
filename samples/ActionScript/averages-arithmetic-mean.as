function mean(vector:Vector.<Number>):Number
{
	var sum:Number = 0;
	for(var i:uint = 0; i < vector.length; i++)
		sum += vector[i];
	return vector.length == 0 ? 0 : sum / vector.length;
}
