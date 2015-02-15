function selectionSort(input: Array):Array {
	//find the i'th element
	for (var i:uint = 0; i < input.length; i++) {
		//set minIndex to an arbitrary value
		var minIndex:uint=i;
		//find the smallest number
		for (var j:uint = i; j < input.length; j++) {
			if (input[j]<input[minIndex]) {
				minIndex=j;
			}
		}
		//swap the smallest number into place
		var tmp:Number=input[i];
		input[i]=input[minIndex];
		input[minIndex]=tmp;
	}
	return input;
}
