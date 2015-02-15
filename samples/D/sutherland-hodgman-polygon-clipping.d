import std.stdio, std.array, std.range, std.typecons, std.algorithm;

struct Vec2 { // To be replaced with Phobos code.
    double x, y;

    Vec2 opBinary(string op="-")(in Vec2 other) const pure nothrow {
        return Vec2(this.x - other.x, this.y - other.y);
    }

    typeof(x) cross(in Vec2 other) const pure nothrow {
        return this.x * other.y - this.y * other.x;
    }
}

immutable(Vec2)[] clip(in Vec2[] subjectPolygon, in Vec2[] clipPolygon)
pure /*nothrow*/ in {
    assert(subjectPolygon.length > 1);
    assert(clipPolygon.length > 1);
    // Probably clipPolygon needs to be convex and probably
    // its vertices need to be listed in a direction.
} out(result) {
    assert(result.length > 1);
} body {
    alias Edge = Tuple!(Vec2,"p", Vec2,"q");

    static enum isInside = (in Vec2 p, in Edge cle) pure nothrow =>
        (cle.q.x - cle.p.x) * (p.y - cle.p.y) >
        (cle.q.y - cle.p.y) * (p.x - cle.p.x);

    static Vec2 intersection(in Edge se, in Edge cle) pure nothrow {
        immutable dc = cle.p - cle.q;
        immutable dp = se.p - se.q;
        immutable n1 = cle.p.cross(cle.q);
        immutable n2 = se.p.cross(se.q);
        immutable n3 = 1.0 / dc.cross(dp);
        return Vec2((n1 * dp.x - n2 * dc.x) * n3,
                    (n1 * dp.y - n2 * dc.y) * n3);
    }

    // How much slower is this compared to lower-level code?
    static enum edges = (in Vec2[] poly) pure nothrow =>
        // poly[$ - 1 .. $].chain(poly).zip!Edge(poly);
        poly[$ - 1 .. $].chain(poly).zip(poly).map!Edge;

    immutable(Vec2)[] result = subjectPolygon.idup; // Not nothrow.

    foreach (immutable clipEdge; edges(clipPolygon)) {
        immutable inputList = result;
        result.clear;
        foreach (immutable inEdge; edges(inputList)) {
            if (isInside(inEdge.q, clipEdge)) {
                if (!isInside(inEdge.p, clipEdge))
                    result ~= intersection(inEdge, clipEdge);
                result ~= inEdge.q;
            } else if (isInside(inEdge.p, clipEdge))
                result ~= intersection(inEdge, clipEdge);
        }
    }

    return result;
}

// Code adapted from the C version.
void saveEPSImage(in string fileName, in Vec2[] subjPoly,
                  in Vec2[] clipPoly, in Vec2[] clipped)
in {
    assert(!fileName.empty);
    assert(subjPoly.length > 1);
    assert(clipPoly.length > 1);
    assert(clipped.length > 1);
} body {
    auto eps = File(fileName, "w");

    // The image bounding box is hard-coded, not computed.
    eps.writeln(
"%%!PS-Adobe-3.0
%%%%BoundingBox: 40 40 360 360
/l {lineto} def
/m {moveto} def
/s {setrgbcolor} def
/c {closepath} def
/gs {fill grestore stroke} def
");

    eps.writef("0 setlinewidth %g %g m ", clipPoly[0].tupleof);
    foreach (immutable cl; clipPoly[1 .. $])
        eps.writef("%g %g l ", cl.tupleof);
    eps.writefln("c 0.5 0 0 s gsave 1 0.7 0.7 s gs");

    eps.writef("%g %g m ", subjPoly[0].tupleof);
    foreach (immutable s; subjPoly[1 .. $])
        eps.writef("%g %g l ", s.tupleof);
    eps.writefln("c 0 0.2 0.5 s gsave 0.4 0.7 1 s gs");

    eps.writef("2 setlinewidth [10 8] 0 setdash %g %g m ",
               clipped[0].tupleof);
    foreach (immutable c; clipped[1 .. $])
        eps.writef("%g %g l ", c.tupleof);
    eps.writefln("c 0.5 0 0.5 s gsave 0.7 0.3 0.8 s gs");

    eps.writefln("%%%%EOF");
    eps.close;
    writeln(fileName, " written.");
}

void main() {
    alias V = Vec2;
    immutable subjectPolygon = [V(50, 150), V(200, 50), V(350, 150),
                                V(350, 300), V(250, 300), V(200, 250),
                                V(150, 350), V(100, 250), V(100, 200)];
    immutable clippingPolygon = [V(100, 100), V(300, 100),
                                 V(300, 300), V(100, 300)];
    immutable clipped = subjectPolygon.clip(clippingPolygon);
    writefln("%(%s\n%)", clipped);
    saveEPSImage("sutherland_hodgman_clipping_out.eps",
                 subjectPolygon, clippingPolygon, clipped);
}
