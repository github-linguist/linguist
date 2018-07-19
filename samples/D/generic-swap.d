import std.algorithm: swap; // from Phobos standard library

// The D solution uses templates and it's similar to the C++ one:
void mySwap(T)(ref T left, ref T right) {
    auto temp = left;
    left = right;
    right = temp;
}

void main() {
    import std.stdio;

    int[] a = [10, 20];
    writeln(a);

    // The std.algorithm standard library module
    // contains a generic swap:
    swap(a[0], a[1]);
    writeln(a);

    // Using mySwap:
    mySwap(a[0], a[1]);
    writeln(a);
}
