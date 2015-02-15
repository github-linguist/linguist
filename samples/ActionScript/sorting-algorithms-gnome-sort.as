function gnomeSort(array:Array)
{
	var pos:uint = 0;
	while(pos < array.length)
	{
		if(pos == 0 || array[pos] >= array[pos-1])
			pos++;
		else
		{
			var tmp = array[pos];
			array[pos] = array[pos-1];
			array[pos-1] = tmp;
			pos--;
		}
	}
	return array;
}
