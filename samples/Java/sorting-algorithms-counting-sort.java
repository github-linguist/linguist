public static void countingSort(int[] array, int min, int max){
	int[] count= new int[max - min + 1];
	for(int number : array){
		count[number - min]++;
	}
	int z= 0;
	for(int i= min;i <= max;i++){
		while(count[i - min] > 0){
			array[z]= i;
			z++;
			count[i - min]--;
		}
	}
}
