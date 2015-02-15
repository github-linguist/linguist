import std.stdio, std.algorithm, std.functional;

string[][] sortTable(string[][] table,
                     in bool function(string[],string[]) ordering=null,
                     in int column = 0,
                     in bool reverse = false) {
    if (ordering is null)
        table.schwartzSort!(row => row[column])();
    else
        table.sort!ordering();
    if (reverse)
        table.reverse();
    return table;
}

void main() {
    auto data = [["a", "b", "c"],
                 ["", "q", "z"],
                 ["zap", "zip", "Zot"]];

    alias show = curry!(writefln, "%-(%s\n%)\n");
    show(data);
    show(sortTable(data));
    show(sortTable(data, null, 2));
    show(sortTable(data, null, 1));
    show(sortTable(data, null, 1, true));
    show(sortTable(data, (a,b) => b.length > a.length));
}
