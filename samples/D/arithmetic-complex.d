import std.stdio, std.complex;

void main() {
    auto x = complex(1, 1); // complex of doubles on default
    auto y = complex(3.14159, 1.2);

    writeln(x + y);   // addition
    writeln(x * y);   // multiplication
    writeln(1.0 / x); // inversion
    writeln(-x);      // negation
}
