// A function:
int multiply1(int a, int b) {
    return a * b;
}

// Functions like "multiply1" can be evaluated at compile time if
// they are called where a compile-time constant result is asked for:
enum result = multiply1(2, 3); // Evaluated at compile time.
int[multiply1(2, 4)] array;    // Evaluated at compile time.

// A templated function:
T multiply2(T)(T a, T b) {
    return a * b;
}

// Compile-time multiplication can also be done using templates:
enum multiply3(int a, int b) = a * b;

pragma(msg, multiply3!(2, 3)); // Prints "6" during compilation.

void main() {
    import std.stdio;
    writeln("2 * 3 = ", result);
}
