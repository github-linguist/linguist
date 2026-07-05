array[5] 1, 2, 3, 4, 5;

sum(array, size){
	auto sum 0;
	auto i 0; while(i < 5){
		sum =+ array[i];
		i++;
	}
	return(sum);
}

main(){
	extrn printf;
	auto i 0; while(i < 5){
		printf("%d*n", array[i]);
		i++;
	}
	printf("sum = %d*n", sum(array, 5));
	return(0);
}
