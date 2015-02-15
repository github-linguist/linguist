import std.stdio, std.array, std.string, std.datetime, std.traits;

/// A history variable.
struct HistoryVariable(T) {
    /// A value in a point in time.
    static struct HistoryValue {
        SysTime time;
        T value;

        // Alternative to the more common toString.
        //void toString(scope void delegate(string) output) const {
        void toString(scope void delegate(const(char)[]) output)const {
            output(format("%s; %s", time, value));
        }
    }

    const(HistoryValue)[] values;

    private void addValue(T value) {
        values ~= HistoryValue(Clock.currTime(), value);
    }

    void opAssign(T value) {
        addValue(value);
    }

    @property T currentValue() const pure nothrow {
        return values.back.value;
    }

    alias currentValue this;

    @property auto history() const pure nothrow {
        return values;
    }

    /**
    Demonstrating D's compile-time reflection features. The member
    functions that are in this 'static if' block would be added for
    types T that are arrays (including strings). */
    static if (isArray!T) {
        // Append-with-assign operator.
        void opOpAssign(string op : "~")(T element) {
            addValue(currentValue ~ element);
        }

        // Similar implementation for other array operators...
    }
}

void main() {
    HistoryVariable!int x;
    x = 1;
    x = 2;
    x = 3;
    writefln("%(%s\n%)\n", x.history);

    HistoryVariable!string s;
    s = "hello";
    s ~= " world";
    s = "goodby";
    writefln("%(%s\n%)", s.history);
}
