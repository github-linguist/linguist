import std.stdio, std.math, std.algorithm;

immutable struct Point { double x, y; }
immutable struct Edge { Point a, b; }
immutable struct Figure {
    string name;
    Edge[] edges;
}

bool contains(in Figure poly, in Point p) pure nothrow {
    static bool raySegI(in Point p, in Edge edge) pure nothrow {
        enum double epsilon = 0.00001;
        with (edge) {
            if (a.y > b.y)
                //swap(a, b); // if edge is mutable
                return raySegI(p, Edge(b, a));
            if (p.y == a.y || p.y == b.y)
                //p.y += epsilon; // if p is mutable
                return raySegI(Point(p.x, p.y + epsilon), edge);
            if (p.y > b.y || p.y < a.y || p.x > max(a.x, b.x))
                return false;
            if (p.x < min(a.x, b.x))
                return true;
            immutable blue = (abs(a.x - p.x) > double.min_normal) ?
                             ((p.y - a.y) / (p.x - a.x)) :
                             double.max;
            immutable red = (abs(a.x - b.x) > double.min_normal) ?
                            ((b.y - a.y) / (b.x - a.x)) :
                            double.max;
            return blue >= red;
        }
    }

    return poly.edges.count!(e => raySegI(p, e))() % 2;
}

void main() {
    immutable Figure[] polys = [
  {"Square", [
    {{ 0.0,  0.0}, {10.0,  0.0}},  {{10.0,  0.0}, {10.0, 10.0}},
    {{10.0, 10.0}, { 0.0, 10.0}},  {{ 0.0, 10.0}, { 0.0,  0.0}}]},
  {"Square hole", [
    {{ 0.0,  0.0}, {10.0,  0.0}},  {{10.0,  0.0}, {10.0, 10.0}},
    {{10.0, 10.0}, { 0.0, 10.0}},  {{ 0.0, 10.0}, { 0.0,  0.0}},
    {{ 2.5,  2.5}, { 7.5,  2.5}},  {{ 7.5,  2.5}, { 7.5,  7.5}},
    {{ 7.5,  7.5}, { 2.5,  7.5}},  {{ 2.5,  7.5}, { 2.5,  2.5}}]},
  {"Strange", [
    {{ 0.0,  0.0}, { 2.5,  2.5}},  {{ 2.5,  2.5}, { 0.0, 10.0}},
    {{ 0.0, 10.0}, { 2.5,  7.5}},  {{ 2.5,  7.5}, { 7.5,  7.5}},
    {{ 7.5,  7.5}, {10.0, 10.0}},  {{10.0, 10.0}, {10.0,  0.0}},
    {{10.0,  0},   { 2.5,  2.5}}]},
  {"Exagon", [
    {{ 3.0,  0.0}, { 7.0,  0.0}},  {{ 7.0,  0.0}, {10.0,  5.0}},
    {{10.0,  5.0}, { 7.0, 10.0}},  {{ 7.0, 10.0}, { 3.0, 10.0}},
    {{ 3.0, 10.0}, { 0.0,  5.0}},  {{ 0.0,  5.0}, { 3.0,  0.0}}]}
];
    immutable Point[] testPoints = [{ 5, 5}, {5, 8}, {-10,  5}, {0, 5},
                                    {10, 5}, {8, 5}, { 10, 10}];

    foreach (immutable poly; polys) {
        writefln(`Is point inside figure "%s"?`, poly.name);
        foreach (immutable p; testPoints)
            writefln("  (%3s, %2s): %s", p.x, p.y, contains(poly, p));
        writeln;
    }
}
