import std.stdio: writeln;
import std.algorithm: startsWith, endsWith, find, countUntil;

void main() {
    writeln("abcd".startsWith("ab"));      // true
    writeln("abcd".endsWith("zn"));        // false
    writeln("abab".find("bb"));            // empty array (no match)
    writeln("abcd".find("bc"));            // "bcd" (substring start
                                           //        at match)
    writeln("abab".countUntil("bb"));      // -1 (no match)
    writeln("abab".countUntil("ba"));      //  1 (index of 1st match)

    // std.algorithm.startsWith also works on arrays and ranges:
    writeln([1, 2, 3].countUntil(3));      //  2
    writeln([1, 2, 3].countUntil([2, 3])); //  1
}
