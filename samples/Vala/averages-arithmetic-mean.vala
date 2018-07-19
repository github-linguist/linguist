double arithmetic(double[] list){
	double mean;
	double sum = 0;
	
	if (list.length == 0)
		return 0.0;
	foreach(double number in list){
		sum += number;
	} // foreach
	
	mean = sum / list.length;
	
	return mean;
} // end arithmetic mean

public static void main(){
	double[] test = {1.0, 2.0, 5.0, -5.0, 9.5, 3.14159};
	double[] zero_len = {};
	
	double mean = arithmetic(test);
	double mean_zero = arithmetic(zero_len);
	
	stdout.printf("%s\n", mean.to_string());
	stdout.printf("%s\n", mean_zero.to_string());
}
