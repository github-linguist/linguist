var hofst_10k = function(n) {
	var memo = [1, 1];

	var a = function(n) {
		var result = memo[n-1];
		if (typeof result !== 'number') {
			result = a(a(n-1))+a(n-a(n-1));	
			memo[n-1] = result;
		}	
		return result;
	}
	return a;
}();

var maxima_between_twos = function(exp) {
	var current_max = 0;
	for(var i = Math.pow(2,exp)+1; i < Math.pow(2,exp+1); i += 1) {
		current_max = Math.max(current_max, hofst_10k(i)/i);
	}
	return current_max;
}

for(var i = 1; i <= 20; i += 1) {
	console.log("Maxima between 2^"+i+"-2^"+(i+1)+" is: "+maxima_between_twos(i)+"\n");
}
