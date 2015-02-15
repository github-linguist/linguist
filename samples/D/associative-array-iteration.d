import std.stdio: writeln;

void main() {
    // the associative array
    auto aa = ["alice":2, "bob":97, "charlie":45];

    // how to iterate key/value pairs:
    foreach (key, value; aa)
        writeln("1) Got key ", key, " with value ", value);
    writeln();

    // how to iterate the keys:
    foreach (key, _; aa)
        writeln("2) Got key ", key);
    writeln();

    // how to iterate the values:
    foreach (value; aa)
        writeln("3) Got value ", value);
    writeln();

    // how to extract the values, lazy:
    foreach (value; aa.byValue())
        writeln("4) Got value ", value);
    writeln();

    // how to extract the keys, lazy:
    foreach (key; aa.byKey())
        writeln("5) Got key ", key);
    writeln();

    // how to extract all the keys:
    foreach (key; aa.keys)
        writeln("6) Got key ", key);
    writeln();

    // how to extract all the values:
    foreach (value; aa.values)
        writeln("7) Got value ", value);
}
