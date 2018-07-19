import std.stdio, std.algorithm, std.range, std.functional;

immutable texts = [
    "this is a numbered list of twelve statements",
    "exactly 3 of the last 6 statements are true",
    "exactly 2 of the even-numbered statements are true",
    "if statement 5 is true, then statements 6 and 7 are both true",
    "the 3 preceding statements are all false",
    "exactly 4 of the odd-numbered statements are true",
    "either statement 2 or 3 is true, but not both",
    "if statement 7 is true, then 5 and 6 are both true",
    "exactly 3 of the first 6 statements are true",
    "the next two statements are both true",
    "exactly 1 of statements 7, 8 and 9 are true",
    "exactly 4 of the preceding statements are true"];

immutable pure bool function(in bool[])[12] predicates = [
    s => s.length == 12,
    s => s[$ - 6 .. $].sum == 3,
    s => s.dropOne.stride(2).sum == 2,
    s => s[4] ? (s[5] && s[6]) : true,
    s => s[1 .. 4].sum == 0,
    s => s.stride(2).sum == 4,
    s => s[1 .. 3].sum == 1,
    s => s[6] ? (s[4] && s[5]) : true,
    s => s[0 .. 6].sum == 3,
    s => s[10] && s[11],
    s => s[6 .. 9].sum == 1,
    s => s[0 .. 11].sum == 4];

void main() {
    enum nStats = predicates.length;

    foreach (immutable n; 0 .. 2 ^^ nStats) {
        bool[nStats] st, matches;
        nStats.iota.map!(i => !!(n & (2 ^^ i))).copy(st[]);
        st[].zip(predicates[].map!(f => f(st)))
            .map!(s_t => s_t[0] == s_t[1]).copy(matches[]);
        if (matches[].sum >= nStats - 1) {
            if (matches[].all)
                ">>> Solution:".writeln;
            else
                writefln("Missed by statement: %d",
                         matches[].countUntil(false) + 1);
            writefln("%-(%s %)", st[].map!q{ "FT"[a] });
        }
    }
}
