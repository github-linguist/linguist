import std.stdio: writeln;

void main() {
    auto collection1 = "ABC";
    foreach (element; collection1)
        writeln(element);

    auto collection2 = [1, 2, 3];
    foreach (element; collection1)
        writeln(element);

    auto collection3 = [1:10, 2:20, 3:30];
    foreach (element; collection3)
        writeln(element);

    foreach (key, value; collection3)
        writeln(key, " ", value);
}
