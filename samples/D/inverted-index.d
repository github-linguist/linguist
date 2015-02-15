import std.stdio, std.algorithm, std.string, std.file, std.regex;

void main() {
    string[][string] index;

    void parseFile(in string fn) {
        if (!exists(fn) || !isFile(fn))
            throw new Exception("File not found");

        foreach (word; readText(fn).splitter(regex(r"\W"))) {
            word = word.toLower();
            if (!index.get(word, null).canFind(fn))
                index[word] ~= fn;
        }
    }

    immutable fileNames = ["inv1.txt", "inv2.txt", "inv3.txt"];
    foreach (fName; fileNames)
        parseFile(fName);

    while (true) {
        writef("\nEnter a word to search for: (q to quit): ");
        immutable w = readln().strip().toLower();
        if (w == "q") {
            writeln("quitting.");
            break;
        }
        if (w in index)
            writefln("'%s' found in%( %).", w, index[w]);
        else
            writefln("'%s' not found.", w);
    }
}
