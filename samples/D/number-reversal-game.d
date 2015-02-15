import std.stdio, std.random, std.string, std.conv, std.algorithm,
       std.range;

void main() {
    auto data = iota(1, 10).array;
    do data.randomShuffle;
    while (data.isSorted);

    int trial;
    while (!data.isSorted) {
        writef("%d: %s How many numbers to flip? ", ++trial, data);
        data[0 .. readln.strip.to!uint].reverse;
    }
    writefln("\nYou took %d attempts.", trial);
}
