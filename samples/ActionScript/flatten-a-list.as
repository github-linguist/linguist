function flatten(input:Array):Array {
	var output:Array = new Array();
	for (var i:uint = 0; i < input.length; i++) {
                //typeof returns "object" when applied to arrays. This line recursively evaluates nested arrays,
                // although it may break if the array contains objects that are not arrays.
		if (typeof input[i]=="object") {
			output=output.concat(flatten(input[i]));
		} else {
			output.push(input[i]);
		}
	}
	return output;
}
