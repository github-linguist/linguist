import std.stdio, std.conv, std.numeric;

struct V3 {
    union {
        immutable static struct { double x, y, z; }
        immutable double[3] v;
    }

    double dot(in V3 rhs) /*@safe*/ const pure nothrow {
        return dotProduct(v, rhs.v);
    }

    V3 cross(in V3 rhs) @safe const pure nothrow {
        return V3(y * rhs.z - z * rhs.y,
                  z * rhs.x - x * rhs.z,
                  x * rhs.y - y * rhs.x);
    }

    string toString() /*@safe*/ const { return text(v); }
}

double scalarTriple(in V3 a, in V3 b, in V3 c)
/*@safe*/ pure nothrow {
    return a.dot(b.cross(c));
    // function vector_products.V3.cross (const(V3) rhs) immutable
    // is not callable using argument types (const(V3)) const
}

V3 vectorTriple(in V3 a, in V3 b, in V3 c) @safe pure nothrow {
    return a.cross(b.cross(c));
}

void main() {
    immutable V3 a = {3, 4, 5},
                 b = {4, 3, 5},
                 c = {-5, -12, -13};
    writeln("a = ", a);
    writeln("b = ", b);
    writeln("c = ", c);
    writeln("a . b = ", a.dot(b));
    writeln("a x b = ", a.cross(b));
    writeln("a . (b x c) = ", scalarTriple(a, b, c));
    writeln("a x (b x c) = ", vectorTriple(a, b, c));
}
