function mergesort(a:Array)
{
	//Arrays of length 1 and 0 are always sorted
	if(a.length <= 1) return a;
	else
	{
		var middle:uint = a.length/2;
		//split the array into two
		var left:Array = new Array(middle);
		var right:Array = new Array(a.length-middle);
		var j:uint = 0, k:uint = 0;
		//fill the left array
		for(var i:uint = 0; i < middle; i++)
			left[j++]=a[i];
		//fill the right array
		for(i = middle; i< a.length; i++)
			right[k++]=a[i];
		//sort the arrays
		left = mergesort(left);
		right = mergesort(right);
		//If the last element of the left array is less than or equal to the first
		//element of the right array, they are in order and don't need to be merged
		if(left[left.length-1] <= right[0])
			return left.concat(right);
		a = merge(left, right);
		return a;
	}
}

function merge(left:Array, right:Array)
{
	var result:Array = new Array(left.length + right.length);
	var j:uint = 0, k:uint = 0, m:uint = 0;
	//merge the arrays in order
	while(j < left.length && k < right.length)
	{
		if(left[j] <= right[k])
			result[m++] = left[j++];
		else
			result[m++] = right[k++];
	}
	//If one of the arrays has remaining entries that haven't been merged, they
	//will be greater than the rest of the numbers merged so far, so put them on the
	//end of the array.
	for(; j < left.length; j++)
		result[m++] = left[j];
	for(; k < right.length; k++)
		result[m++] = right[k];
	return result;
}
