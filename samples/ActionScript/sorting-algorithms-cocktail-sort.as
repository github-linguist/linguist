function cocktailSort(input:Array):Array {
   do {
        var swapped:Boolean=false;
	for (var i:uint = 0; i < input.length-1; i++) {
	    if (input[i]>input[i+1]) {
	    var tmp=input[i];
	    input[i]=input[i+1];
	    input[i+1]=tmp;
	    swapped=true;
	    }
	}
	if (! swapped) {
            break;
	}
	for (i = input.length -2; i >= 0; i--) {
	    if (input[i]>input[i+1]) {
	    tmp=input[i];
	    input[i]=input[i+1];
	    input[i+1]=tmp;
	    swapped=true;
	    }
        }
    } while (swapped);
   return input;
}
