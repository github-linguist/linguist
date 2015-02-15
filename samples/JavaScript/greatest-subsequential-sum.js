function MaximumSubsequence( population ) {
	var maxValue = 0;
	var subsequence = [];
	
	for( var i=0, len=population.length; i < len; i++ ) {
		for( var j=i; j <= len; j++ ) {
			var subsequence = population.slice(i,j);
			var value = sumValues(subsequence);
			if( value > maxValue ) {
				maxValue = value;
				greatest = subsequence;
			};
		}
	}
		
	return greatest;
}

function sumValues(arr) {
	var result = 0;
	for( var i=0, len=arr.length; i < len; i++) {
		result += arr[i];
	}
	return result;	
}
