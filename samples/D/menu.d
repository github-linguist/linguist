import std.stdio, std.conv, std.string, std.array, std.typecons;

string menuSelect(in string[] entries) {
    static Nullable!(int, -1) validChoice(in string input,
                                          in int nEntries)
    pure nothrow {
        try {
            immutable n = input.to!int;
            return typeof(return)((n >= 0 && n <= nEntries) ? n : -1);
        } catch (Exception e) // Very generic
            return typeof(return)(-1); // Not valid.
    }

    if (entries.empty)
        return "";

    while (true) {
        "Choose one:".writeln;
        foreach (immutable i, const entry; entries)
            writefln("  %d) %s", i, entry);
        "> ".write;
        immutable input = readln.chomp;
        immutable choice = validChoice(input, entries.length - 1);
        if (choice.isNull)
            "Wrong choice.".writeln;
        else
            return entries[choice]; // We have a valid choice.
    }
}

void main() {
    immutable items = ["fee fie", "huff and puff",
                       "mirror mirror", "tick tock"];
    writeln("You chose '", items.menuSelect, "'.");
}
