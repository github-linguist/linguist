void main(){
    int	a;
    int	b;

    stdout.printf("Please type in int 1\n");
    a = int.parse(stdin.read_line());

    stdout.printf("Please type in int 2\n");
    b =	int.parse(stdin.read_line());

    if (a < b)
        stdout.printf("%d is less than %d\n", a, b);
    if (a == b)
        stdout.printf("%d is equal to %d\n", a,	b);
    if (a > b)
        stdout.printf("%d is greater than %d\n", a, b);
}
