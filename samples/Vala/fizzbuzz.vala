int main() {
	for(int i = 1; i < 100; i++) {
		if(i % 3 == 0) stdout.printf("Fizz");
		if(i % 5 == 0) stdout.printf("Buzz");
		if(i % 3 != 0 && i % 5 != 0) stdout.printf("%d", i);
		stdout.printf("\n");
	}
	return 0;
}
