int lcm(int a, int b){
    /*Return least common multiple of two ints*/
    // check for 0's
    if (a == 0 || b == 0)
	return 0;

    // Math.abs(x) only works for doubles, Math.absf(x) for floats
    if (a < 0)
        a *= -1;
    if (b < 0)
	b *= -1;

    int x = 1;
    while (true){
        if (a * x % b == 0)
            return a*x;
        x++;
    }
}

void main(){
    int	a = 12;
    int	b = 18;

    stdout.printf("lcm(%d, %d) = %d\n",	a, b, lcm(a, b));
}
