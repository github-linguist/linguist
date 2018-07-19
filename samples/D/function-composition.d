import std.stdio;

T delegate(S) compose(T, U, S)(in T delegate(U) f,
                               in U delegate(S) g) {
    return s => f(g(s));
}

void main() {
    writeln(compose((int x) => x + 15, (int x) => x ^^ 2)(10));
    writeln(compose((int x) => x ^^ 2, (int x) => x + 15)(10));
}
