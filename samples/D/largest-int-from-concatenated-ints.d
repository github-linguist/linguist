import std.stdio, std.algorithm, std.conv, std.array, permutations2;

auto maxCat1(in int[] arr) {
    return arr.to!(string[]).permutations.map!join.reduce!max;
}

auto maxCat2(in int[] arr) {
    return arr.to!(string[]).sort!q{b ~ a < a ~ b}.join;
}

auto maxCat3(in int[] arr) {
    immutable maxl = arr.reduce!max.text.length;
    return arr.to!(string[])
           .schwartzSort!(s => s.replicate(maxl/s.length + 1), "a > b")
           .join;
}

void main() {
    const lists = [[1, 34, 3, 98, 9, 76, 45, 4], [54, 546, 548, 60]];
    [&maxCat1, &maxCat2, &maxCat3].map!(cat => lists.map!cat).writeln;
}
