import std.stdio: writeln;

void main() {
    int[] a = [1, 2];
    int[] b = [4, 5, 6];

    writeln(a, " ~ ", b, " = ", a ~ b);
}
