import std.stdio, std.algorithm, std.range, std.array;

struct Prop {
    int[string] data;
    ref opDispatch(string s)() pure nothrow {
        return data[s];
    }
}

immutable code = `
writef("% 4d", e.seq);
if (e.seq != 1) {
    e.cnt++;
    e.seq = (e.seq & 1) ? 3 * e.seq + 1 : e.seq / 2;
}`;

void main() {
    auto envs = 12.iota.map!(i => Prop(["cnt": 0, "seq": i+1])).array;

    while (envs.any!(env => env.seq > 1)) {
        foreach (e; envs) {
            mixin(code);
        }
        writeln;
    }

    writefln("Counts:\n%(% 4d%)", envs.map!(env => env.cnt));
}
