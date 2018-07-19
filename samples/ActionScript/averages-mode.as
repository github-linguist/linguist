function Mode(arr:Array):Array {
	//Create an associative array to count how many times each element occurs,
	//an array to contain the modes, and a variable to store how many times each mode appears.
	var count:Array = new Array();
	var modeList:Array;
	var maxCount:uint=0;
	for (var i:String in arr) {
		//Record how many times an element has occurred. Note that each element in the cuont array
		//has to be initialized explicitly, since it is an associative array.
		if (count[arr[i]]==undefined) {
			count[arr[i]]=1;
		} else {
			count[arr[i]]++;
		}
		//If this is now the most common element, clear the list of modes, and add this element.
		if(count[arr[i]] > maxCount)
		{
			maxCount=count[arr[i]];
			modeList = new Array();
			modeList.push(arr[i]);
		}
		//If this is a mode, add it to the list.
		else if(count[arr[i]] == maxCount){
			modeList.push(arr[i]);
		}
	}
	return modeList;
}
