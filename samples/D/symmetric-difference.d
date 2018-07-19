import std.stdio, std.algorithm, std.array;

struct Set(T) {
    immutable T[] items;

    Set opSub(in Set other) const pure nothrow {
        return items.filter!(x => !other.items.canFind(x)).array.Set;
    }

    Set opAdd(in Set other) const pure nothrow {
        return Set(this.items ~ (other - this).items);
    }
}

Set!T symmetricDifference(T)(in Set!T left, in Set!T right)
pure nothrow {
    return (left - right) + (right - left);
}

void main() {
    immutable A = ["John", "Bob", "Mary", "Serena"].Set!string;
    immutable B = ["Jim", "Mary", "John", "Bob"].Set!string;

    writeln("        A\\B: ", (A - B).items);
    writeln("        B\\A: ", (B - A).items);
    writeln("A symdiff B: ", symmetricDifference(A, B).items);
}
