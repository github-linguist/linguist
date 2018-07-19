function insertionSort(array:Array)
{
	for(var i:int = 1; i < array.length;i++)
	{
		var value = array[i];
		var j:int = i-1;
		while(j >= 0 && array[j] > value)
		{
			array[j+1] = array[j];
			j--;
		}
		array[j+1] = value;
	}
	return array;
}
