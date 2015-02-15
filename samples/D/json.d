import std.stdio, std.json;

void main() {
    auto j = parseJSON("{ \"foo\": 1, \"bar\": [10, \"apples\"] }");
    writeln(toJSON(&j));
}
