import std.stdio;

double sum(ref int i, in int lo, in int hi, lazy double term)
pure @safe {
    double result = 0.0;
    for (i = lo; i <= hi; i++)
        result += term();
    return result;
}

void main() {
    int i;
    writeln(sum(i, 1, 100, 1.0/i));
}
