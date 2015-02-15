import std.stdio, std.conv, std.algorithm, std.array;

struct Node(T) { T value; Node* left, right; }

string[] treeIndent(T)(in Node!T* t) {
    if (!t) return ["-- (null)"];
    const tr = t.right.treeIndent;
    return "--" ~ t.value.text ~
           t.left.treeIndent.map!q{"  |" ~ a}.array ~
           ("  `" ~ tr[0]) ~ tr[1 .. $].map!q{"   " ~ a}.array;
}

void main () {
    static N(T)(T v, Node!T* l=null, Node!T* r=null) {
        return new Node!T(v, l, r);
    }

    const tree = N(1, N(2, N(4, N(7)), N(5)), N(3, N(6, N(8), N(9))));
    writefln("%-(%s\n%)", tree.treeIndent);
}
