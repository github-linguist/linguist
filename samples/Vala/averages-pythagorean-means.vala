double arithmetic(int[] list){
	double mean;
	double sum = 0;
	foreach(int number in list){
		sum += number;
	} // foreach
	
	mean = sum / list.length;
	
	return mean;
} // end arithmetic mean

double geometric(int[] list){
	double mean;
	double product = 1;
	foreach(int number in list){
		product *= number;
	} // foreach

	mean = Math.pow(product, (1 / (double) list.length));
	
	return mean;
} // end geometric mean

double harmonic(int[] list){
	double mean;
	double sum_inverse = 0;
	foreach(int number in list){
		sum_inverse += (1 / (double) number);
	} // foreach
	
	mean = (double) list.length / sum_inverse;
	
	return mean;
} // end harmonic mean

public static void main(){
	int[] list = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
	
	double arithmetic_mean = arithmetic(list);
	double geometric_mean = geometric(list);
	double harmonic_mean = harmonic(list);
	
	// should be 5.5
	stdout.printf("Arithmetic mean: %s\n", arithmetic_mean.to_string());
	
	// should be 4.528728688116765
	stdout.printf("Geometric mean: %s\n", geometric_mean.to_string());
	
	// should be 4.528728688116765
	stdout.printf("Harmonic mean: %s\n", harmonic_mean.to_string());
}
