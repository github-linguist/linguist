import std.stdio: writeln;

void shellSort(T)(T[] seq) pure nothrow {
    int inc = seq.length / 2;
    while (inc) {
        foreach (ref i, el; seq) {
            while (i >= inc && seq[i - inc] > el) {
                seq[i] = seq[i - inc];
                i -= inc;
            }
            seq[i] = el;
        }
        inc = (inc == 2) ? 1 : cast(int)(inc * 5.0 / 11);
    }
}

void main() {
    auto data = [22, 7, 2, -5, 8, 4];
    shellSort(data);
    writeln(data);
}
