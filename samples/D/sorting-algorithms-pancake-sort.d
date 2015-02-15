import std.stdio, std.algorithm;

void pancakeSort(bool tutor=false, T)(T[] data) {
    foreach_reverse (i; 2 .. data.length + 1) {
        immutable maxIndex = i - data[0 .. i].minPos!q{a > b}().length;
        if (maxIndex + 1 != i) {
            if (maxIndex != 0) {
                static if (tutor)
                    writeln("With: ", data, " doflip ", maxIndex + 1);
                data[0 .. maxIndex + 1].reverse();
            }

            static if (tutor)
                writeln("With: ", data, " doflip ", i);
            data[0 .. i].reverse();
        }
    }
}

void main() {
    char[] data = "769248135".dup;
    pancakeSort!true(data);
    writeln(data);
}
