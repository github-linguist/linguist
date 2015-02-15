int main() {
	int x = Random.int_range(1, 10);
	stdout.printf("Make a guess (1-10): ");
	while(int.parse(stdin.read_line()) != x)
                stdout.printf("Wrong! Try again: ");
	stdout.printf("Got it!\n");
	return 0;
}
