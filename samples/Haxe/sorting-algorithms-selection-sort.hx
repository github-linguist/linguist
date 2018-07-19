static function selectionSort(arr:Array<Int>) {
	var len = arr.length;
	for (index in 0...len)
	{
		var minIndex = index;
		for (remainingIndex in (index+1)...len)
		{
			if (arr[minIndex] > arr[remainingIndex]) {
				minIndex = remainingIndex;
			}
		}
		if (index != minIndex) {
			var temp = arr[index];
			arr[index] = arr[minIndex];
			arr[minIndex] = temp;
		}
	}
}
