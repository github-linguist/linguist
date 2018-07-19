import std.traits;

S stripChars(S)(S s, bool function(dchar) pure nothrow mustStrip)
        pure nothrow if (isSomeString!S) {
    S result;
    foreach (c; s) {
        if (!mustStrip(c))
            result ~= c;
    }
    return result;
}

void main() {
    import std.stdio, std.uni;
    auto s = "\u0000\u000A abc\u00E9def\u007F";
    writeln(s.stripChars( &isControl ));
    writeln(s.stripChars( c => isControl(c) || c == '\u007F' ));
    writeln(s.stripChars( c => isControl(c) || c >= '\u007F' ));
}
