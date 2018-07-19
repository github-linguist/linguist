public static void main(){
	int sum = 0, product = 1;
	
	int[] array = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
	
	foreach (int number in array){
		sum += number;
		product *= number;
	}
}
