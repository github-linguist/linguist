import std.stdio;

void main() {
    int countLines;
    char[] ln;
    auto f = File("linenumber.d", "r");
    foreach (char[] line; f.byLine()) {
        countLines++;
        if (countLines == 7) {
            ln = line;
            break;
        }
    }
    switch(countLines) {
        case 0 : writeln("the file has zero length");
                break;
        case 7 : writeln("line 7: ", (ln.length ? ln : "empty"));
                break;
        default :
                writefln("the file only contains %d lines", countLines);
    }
}
