//recursively builds the permutations of permutable, appended to front, and returns the first sorted permutation it encounters
function permutations(front:Array, permutable:Array):Array {
	//If permutable has length 1, there is only one possible permutation. Check whether it's sorted
	if (permutable.length==1)
		return isSorted(front.concat(permutable));
	else
		//There are multiple possible permutations. Generate them.
		var i:uint=0,tmp:Array=null;
		do
		{
			tmp=permutations(front.concat([permutable[i]]),remove(permutable,i));
			i++;
		}while (i< permutable.length && tmp == null);
		//If tmp != null, it contains the sorted permutation. If it does not contain the sorted permutation, return null. Either way, return tmp.
		return tmp;
}
//returns the array if it's sorted, or null otherwise
function isSorted(data:Array):Array {
	for (var i:uint = 1; i < data.length; i++)
		if (data[i]<data[i-1])
			return null;
	return data;
}
//returns a copy of array with the i'th element removed
function remove(array:Array, i:uint):Array {
	return array.filter(function(item,index,array){return(index !=i)}) ;
}
//wrapper around the permutation function to provide a more logical interface
function permutationSort(array:Array):Array {
	return permutations([],array);
}
