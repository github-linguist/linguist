void main() {
    // A normal POD struct
    // (if it's nested and it's not static then it has a hidden
    // field that points to the enclosing function):
    static struct Point {
        int x, y;
    }

    auto p1 = Point(10, 20);

    // It can also be parametrized on the coordinate type:
    static struct Pair(T) {
        T x, y;
    }

    // A pair with integer coordinates:
    auto p2 = Pair!int(3, 5);

    // A pair with floating point coordinates:
    auto p3 = Pair!double(3, 5);

    // Classes (static inner):
    static class PointClass {
        int x, y;
        this(int x_, int y_) {
            this.x = x_;
            this.y = y_;
        }
    }

    auto p4 = new PointClass(1, 2);

    // There are also library-defined tuples:
    import std.typecons;

    alias Tuple!(int,"x", int,"y") PointXY;

    auto p5 = PointXY(3, 5);

    // And even built-in "type tuples":
    import std.typetuple;

    alias TypeTuple!(int, 5) p6;

    static assert(is(p6[0] == int));
    static assert(p6[1] == 5);
}
