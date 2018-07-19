public static void cocktailSort( int[] A ){
	boolean swapped;
	do {
		swapped = false;
		for (int i =0; i<=  A.length  - 2;i++) {
			if (A[ i ] > A[ i + 1 ]) {
				//test whether the two elements are in the wrong order
				int temp = A[i];
				A[i] = A[i+1];
				A[i+1]=temp;
				swapped = true;
			}
		}
		if (!swapped) {
			//we can exit the outer loop here if no swaps occurred.
			break;
		}
		swapped = false;
		for (int i= A.length - 2;i>=0;i--) {
			if (A[ i ] > A[ i + 1 ]) {
				int temp = A[i];
				A[i] = A[i+1];
				A[i+1]=temp;
				swapped = true;
			}
		}
		//if no elements have been swapped, then the list is sorted
	} while (swapped);
}
