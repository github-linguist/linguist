class ArrayTree(T, uint N) {
    T[N] data;
    typeof(this) left, right;

    this(T initValue) { this.data[] = initValue; }

    void tmap(const void delegate(ref typeof(data)) dg) {
        dg(this.data);
        if (left) left.tmap(dg);
        if (right) right.tmap(dg);
    }
}

void main() { // Demo code.
    import std.stdio;

    // Instantiate the template ArrayTree of three doubles.
    alias AT3 = ArrayTree!(double, 3);

    // Allocate the tree root.
    auto root = new AT3(1.00);

    // Add some nodes.
    root.left = new AT3(1.10);
    root.left.left = new AT3(1.11);
    root.left.right = new AT3(1.12);

    root.right = new AT3(1.20);
    root.right.left = new AT3(1.21);
    root.right.right = new AT3(1.22);

    // Now the tree has seven nodes.

    // Show the arrays of the whole tree.
    //root.tmap(x => writefln("%(%.2f %)", x));
    root.tmap((ref x) => writefln("%(%.2f %)", x));

    // Modify the arrays of the whole tree.
    //root.tmap((x){ x[] += 10; });
    root.tmap((ref x){ x[] += 10; });

    // Show the arrays of the whole tree again.
    writeln();
    //root.tmap(x => writefln("%(%.2f %)", x));
    root.tmap((ref x) => writefln("%(%.2f %)", x));
}
