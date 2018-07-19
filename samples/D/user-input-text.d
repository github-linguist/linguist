import std.stdio;

void main() {
    long number;
    write("Enter an integer: ");
    readf("%d", &number);

    char[] str;
    write("Enter a string: ");
    readf(" %s\n", &str);

    writeln("Read in '", number, "' and '", str, "'");
}
