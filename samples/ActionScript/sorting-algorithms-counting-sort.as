function countingSort(array:Array, min:int, max:int)
{
	var count:Array = new Array(array.length);
	for(var i:int = 0; i < count.length;i++)count[i]=0;
	for(i = 0; i < array.length; i++)
	{
		count[array[i]-min] ++;
	}
	var j:uint = 0;
	for(i = min; i <= max; i++)
	{
		for(; count[i-min] > 0; count[i-min]--)
			array[j++] = i;
	}
	return array;
}
