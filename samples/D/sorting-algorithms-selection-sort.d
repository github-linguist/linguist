import std.stdio, std.algorithm, std.array, std.traits;

enum AreSortableArrayItems(T) = isMutable!T &&
                                __traits(compiles, T.init < T.init) &&
                                !isNarrowString!(T[]);

void selectionSort(T)(T[] data) if (AreSortableArrayItems!T) {
    foreach (immutable i, ref d; data)
        data.drop(i).minPos[0].swap(d);
} unittest {
    int[] a0;
    a0.selectionSort;

    auto a1 = [1];
    a1.selectionSort;
    assert(a1 == [1]);

    auto a2 = ["a", "b"];
    a2.selectionSort;
    assert(a2 == ["a", "b"]);

    auto a3 = ["b", "a"];
    a3.selectionSort;
    assert(a3 == ["a", "b"]);

    auto a4 = ['a', 'b'];
    static assert(!__traits(compiles, a4.selectionSort));

    dchar[] a5 = ['b', 'a'];
    a5.selectionSort;
    assert(a5 == "ab"d);

    import std.typecons;
    alias Nullable!int N;
    auto a6 = [N(2), N(1)];
    a6.selectionSort; // Not nothrow.
    assert(a6 == [N(1), N(2)]);

    auto a7 = [1.0+0i, 2.0+0i]; // To be deprecated.
    static assert(!__traits(compiles, a7.selectionSort));

    import std.complex;
    auto a8 = [complex(1), complex(2)];
    static assert(!__traits(compiles, a8.selectionSort));

    static struct F {
        int x;
        int opCmp(F f) { // Not pure.
            return x < f.x ? -1 : (x > f.x ? 1 : 0);
        }
    }
    auto a9 = [F(2), F(1)];
    a9.selectionSort;
    assert(a9 == [F(1), F(2)]);
}

void main() {
    auto a = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9, 3, 2];
    a.selectionSort;
    a.writeln;
}
