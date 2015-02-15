import std.stdio, std.string, std.conv, std.regex, std.getopt;

enum VarName(alias var) = var.stringof.toUpper;

void setOpt(alias Var)(in string line) {
    auto m = match(line, regex(`^` ~ VarName!Var ~ `(\s+(.*))?`));
    if (!m.empty) {
        static if (is(typeof(Var) == string))
            Var = m.captures.length > 2 ? m.captures[2] : "";
        static if (is(typeof(Var) == bool))
            Var = true;
        static if (is(typeof(Var) == int))
            Var = m.captures.length > 2 ? to!int(m.captures[2]) : 0;
    }
}

void main(in string[] args) {
    string fullName, favouriteFruit, otherFamily;
    bool needsPeeling, seedsRemoved; // Default false.
    auto f = "readcfg.txt".File;

    foreach (line; f.byLine) {
        auto opt = line.strip.idup;
        setOpt!fullName(opt);
        setOpt!favouriteFruit(opt);
        setOpt!needsPeeling(opt);
        setOpt!seedsRemoved(opt);
        setOpt!otherFamily(opt);
    }

    writefln("%14s = %s", VarName!fullName, fullName);
    writefln("%14s = %s", VarName!favouriteFruit, favouriteFruit);
    writefln("%14s = %s", VarName!needsPeeling, needsPeeling);
    writefln("%14s = %s", VarName!seedsRemoved, seedsRemoved);
    writefln("%14s = %s", VarName!otherFamily, otherFamily);
}
