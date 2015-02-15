import std.stdio, std.traits;

enum Node.FP differenceThreshold = 1e-40;

struct Node {
    alias real FP;
    enum Kind : size_t { free, A, B }

    FP voltage = 0.0;
    private Kind kind = Kind.free;

    @property Kind fixed() const pure nothrow { return kind; }
}

Node.FP iter(size_t w, size_t h)(ref Node[w][h] m) pure nothrow {
    static void enforceBoundaryConditions(ref Node[w][h] m)
    pure nothrow {
        m[1][1].voltage =  1.0;
        m[6][7].voltage = -1.0;
    }

    static Node.FP calcDifference(in ref Node[w][h] m,
                                  ref Node[w][h] d) pure nothrow {
        Node.FP total = 0.0;

        foreach (i; 0 .. h)
            foreach (j; 0 .. w) {
                Node.FP v = 0.0;
                {
                    size_t n = 0;
                    if (i != 0)  { v += m[i - 1][j].voltage; n++; }
                    if (j != 0)  { v += m[i][j - 1].voltage; n++; }
                    if (i < h-1) { v += m[i + 1][j].voltage; n++; }
                    if (j < w-1) { v += m[i][j + 1].voltage; n++; }
                    v = m[i][j].voltage - v / n;
                }

                d[i][j].voltage = v;
                if (m[i][j].fixed == Node.Kind.free)
                    total += v ^^ 2;
            }

        return total;
    }

    Node[w][h] difference;

    while (true) {
        enforceBoundaryConditions(m);
        if (calcDifference(m, difference) < differenceThreshold)
            break;
        foreach (i, const(Node[]) di; difference)
            foreach (j, ref const(Node) dij; di)
                m[i][j].voltage -= dij.voltage;
    }

    Node.FP[EnumMembers!(Node.Kind).length] cur = 0.0;
    foreach (i, const(Node[]) di; difference)
        foreach (j, ref const(Node) dij; di)
            cur[m[i][j].fixed] += dij.voltage *
                (!!i + !!j + (i < h-1) + (j < w-1));

    return (cur[Node.Kind.A] - cur[Node.Kind.B]) / 2.0;
}

void main() {
    enum size_t w = 10,
                h = w;
    Node[w][h] mesh;

    // Set A and B Nodes.
    mesh[1][1] = Node( 1.0, Node.Kind.A);
    mesh[6][7] = Node(-1.0, Node.Kind.B);

    writefln("R = %.19f", 2 / iter(mesh));
}
