import std.stdio, std.math, std.string, std.typecons, std.traits;

const struct Imprecise {
    private const double value, delta;

    this(in double v, in double d) pure nothrow {
        this.value = v;
        this.delta = abs(d);
    }

    enum IsImprecise(T) = is(Unqual!T == Unqual!(typeof(this)));

    I reciprocal() const pure nothrow {
        return I(1.0 / value, delta / (value ^^ 2));
    }

    string toString() const {
        return format("I(value=%g, delta=%g)", value, delta);
    }

    I opUnary(string op:"-")() const pure nothrow {
        return I(-this.value, this.delta);
    }

    I opBinary(string op:"+", T)(in T other) const pure nothrow
    if (isNumeric!T || IsImprecise!T) {
        static if (IsImprecise!T)
            return I(this.value + other.value,
                     (this.delta ^^ 2 + other.delta ^^ 2) ^^ 0.5);
        else
            return I(this.value + other, this.delta);
    }

    I opBinaryRight(string op:"+", T)(in T other) const pure nothrow
    if (isNumeric!T) {
        return I(this.value + other, this.delta);
    }

    I opBinary(string op:"-", T)(in T other) const pure nothrow
    if (isNumeric!T || IsImprecise!T) {
        return this + (-other);
    }

    I opBinaryRight(string op:"-", T)(in T other) const pure nothrow
    if (isNumeric!T) {
        return this - other;
    }

    I opBinary(string op:"*", T)(in T other) const pure nothrow
    if (isNumeric!T || IsImprecise!T) {
        static if (IsImprecise!T) {
            auto f = this.value * other.value;
            return I(f, f * ((delta / value) ^^ 2 +
                     (other.delta / other.value) ^^ 2) ^^ 0.5);
        } else
            return I(this.value * other, this.delta * other);
    }

    I opBinaryRight(string op:"*", T)(in T other) const pure nothrow
    if (isNumeric!T) {
        return this * other;
    }

    I opBinary(string op:"/", T)(in T other) const pure nothrow
    if (isNumeric!T || IsImprecise!T) {
        static if (IsImprecise!T)
            return this * other.reciprocal();
        else
            return I(this.value / other, this.delta / other);
    }

    I opBinaryRight(string op:"/", T)(in T other) const pure nothrow
    if (isNumeric!T) {
        return this / other;
    }

    I opBinary(string op:"^^", T)(in T other) const pure nothrow
    if (isNumeric!T) {
        auto f = this.value ^^ other;
        return I(f, f * other * (this.delta / this.value));
    }
}

alias I = Imprecise;

auto distance(T1, T2)(in T1 p1, in T2 p2) pure nothrow {
    return ((p1[0] - p2[0]) ^^ 2 + (p1[1] - p2[1]) ^^ 2) ^^ 0.5;
}

void main() {
    immutable x1 = I(100, 1.1);
    immutable x2 = I(200, 2.2);
    immutable y1 = I( 50, 1.2);
    immutable y2 = I(100, 2.3);

    immutable p1 = tuple(x1, y1);
    immutable p2 = tuple(x2, y2);
    writefln("Point p1: (%s, %s)", p1[0], p1[1]);
    writefln("Point p2: (%s, %s)", p2[0], p2[1]);
    writeln("Distance(p1, p2): ", distance(p1, p2));
}
