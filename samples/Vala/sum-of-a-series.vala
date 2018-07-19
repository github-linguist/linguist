public static void main(){
	int i, start = 1, end = 1000;
	double sum = 0.0;
	
	for(i = start; i<= end; i++)
		sum += (1 / (double)(i * i));
	
	stdout.printf("%s\n", sum.to_string());
}
