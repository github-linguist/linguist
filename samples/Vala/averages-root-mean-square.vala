double rms(double[] list){
	double sum_squares = 0;
	double mean;
	
	foreach ( double number in list){
		sum_squares += (number * number);
	}
	
	mean = Math.sqrt(sum_squares / (double) list.length);
	
	return mean;
} // end rms

public static void main(){
	double[] list = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
	double mean = rms(list);
	
	stdout.printf("%s\n", mean.to_string());
}
