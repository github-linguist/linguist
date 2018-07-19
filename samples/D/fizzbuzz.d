import std.stdio: writeln;

// with if-else
void fizzBuzz(int n) {
    foreach (i; 1 .. n+1)
        if (!(i % 15))
            writeln("FizzBuzz");
        else if (!(i % 3))
            writeln("Fizz");
        else if (!(i % 5))
            writeln("Buzz");
        else
            writeln(i);
}

// with switch case
void fizzBuzzSwitch(int n) {
    foreach (i; 1 .. n+1)
        switch(i % 15) {
            case 0:
                writeln("FizzBuzz"); break;
            case 3, 6, 9, 12:
                writeln("Fizz"); break;
            case 5, 10:
                writeln("Buzz"); break;
            default:
                writeln(i);
        }
}

void main() {
    fizzBuzz(100);
    writeln();
    fizzBuzzSwitch(100);
}
