import std.stdio, std.conv;

void main() {
    writeln("oct: ", octal!777);
    writeln("bin: ", 0b01011010);
    writeln("hex: ", 0xBADF00D);
    writeln("dec: ", 1000000000);
    writeln("dec: ", 1_000_000_000);
    writeln();

    writeln(typeid(typeof(0)));
    writeln(typeid(typeof(0u)));
    // writeln(typeid(typeof(0l))); // 'l' suffix is deprecated
    writeln(typeid(typeof(0L)));
    writeln(typeid(typeof(0uL)));
    writeln(typeid(typeof(0LU)));
    writeln();

    writefln("%x", 0xFEE1_BAD_CAFE_BABEuL);
}
