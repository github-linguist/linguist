function mult(num1,num2){
	var a1 = num1.split("").reverse();
	var a2 = num2.split("").reverse();
	var aResult = new Array;
	
	for ( iterNum1 = 0; iterNum1 < a1.length; iterNum1++ ) {
		for ( iterNum2 = 0; iterNum2 < a2.length; iterNum2++ ) {
			idxIter = iterNum1 + iterNum2;	// Get the current array position.
			aResult[idxIter] = a1[iterNum1] * a2[iterNum2] + ( idxIter >= aResult.length ? 0 : aResult[idxIter] );
			
			if ( aResult[idxIter] > 9 ) {	// Carrying
				aResult[idxIter + 1] = Math.floor( aResult[idxIter] / 10 ) + ( idxIter + 1 >= aResult.length ? 0 : aResult[idxIter + 1] );
				aResult[idxIter] -= Math.floor( aResult[idxIter] / 10 ) * 10;
			}
		}
	}
	return aResult.reverse().join("");
}
