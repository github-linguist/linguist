function sumOfSquares(vector:Vector.<Number>):Number
{
	var sum:Number = 0;
	for(var i:uint = 0; i < vector.length; i++)
		sum += vector[i]*vector[i];
	return sum;
}
