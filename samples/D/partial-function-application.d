import std.stdio, std.algorithm, std.traits;

auto fs(alias f)(in int[] s) pure nothrow
if (isCallable!f && ParameterTypeTuple!f.length == 1) {
    return s.map!f;
}

int f1(in int x) pure nothrow { return x * 2; }
int f2(in int x) pure nothrow { return x ^^ 2; }

alias fsf1 = fs!f1;
alias fsf2 = fs!f2;

void main() {
    foreach (const d; [[0, 1, 2, 3], [2, 4, 6, 8]]) {
        d.fsf1.writeln;
        d.fsf2.writeln;
    }
}
