import std.stdio, std.algorithm, std.range, std.array, std.conv;
import combinations4: Comb;

alias iRNG = int[];

iRNG setDiff(iRNG s, iRNG c) {
    return setDifference(s, c).array;
}

iRNG[][] orderPart(iRNG blockSize...) {
    iRNG sum = iota(1, 1 + blockSize.sum).array;

    iRNG[][] p(iRNG s, in iRNG b) {
        if (b.length == 0)
            return [[]];
        iRNG[][] res;
        foreach (c; Comb.On(s, b[0]))
            foreach (r; p(setDiff(s, c), b.dropOne))
                res ~= c.dup ~ r;
        return res;
    }

    return p(sum, blockSize);
}

void main(in string[] args) {
    auto b = args.length > 1 ? args.dropOne.to!(int[]) : [2, 0, 2];
    writefln("%(%s\n%)", b.orderPart);
}
