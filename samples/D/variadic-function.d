import std.stdio, std.algorithm;

void printAll(TyArgs...)(TyArgs args) {
    foreach (el; args)
        el.writeln;
}

// Typesafe variadic function for dynamic array
void showSum1(int[] items...) {
    items.sum.writeln;
}

// Typesafe variadic function for fixed size array
void showSum2(int[4] items...) {
    items[].sum.writeln;
}

void main() {
    printAll(4, 5.6, "Rosetta", "Code", "is", "awseome");
    writeln;
    showSum1(1, 3, 50);
    showSum2(1, 3, 50, 10);
}
