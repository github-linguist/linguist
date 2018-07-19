import std.stdio, std.algorithm, std.array, std.string, std.range;

T pop(T)(ref T[] items, in size_t i) pure {
    auto aux = items[i];
    items.remove(i);
    items.length--;
    return aux;
}

string josephus(in int n, in int k) pure {
    auto p = n.iota.array;
    int i;
    int[] seq;
    while (!p.empty) {
        i = (i + k - 1) % p.length;
        seq ~= p.pop(i);
    }

    return format("Prisoner killing order:\n%(%(%d %)\n%)." ~
                  "\nSurvivor: %d",
                  seq[0 .. $ - 1].chunks(20), seq[$ - 1]);
}

void main() {
    josephus(5, 2).writeln;
    writeln;
    josephus(41, 3).writeln;
}
