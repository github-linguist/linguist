import std.stdio, std.math;

struct StdDev {
    real sum = 0.0, sqSum = 0.0;
    long nvalues;

    void addNumber(in real input) pure nothrow {
        nvalues++;
        sum += input;
        sqSum += input ^^ 2;
    }

    real getStdDev() const pure nothrow {
        if (nvalues == 0)
            return 0.0;
        immutable real mean = sum / nvalues;
        return sqrt(sqSum / nvalues - mean ^^ 2);
    }
}

void main() {
    StdDev stdev;

    foreach (el; [2.0, 4, 4, 4, 5, 5, 7, 9]) {
        stdev.addNumber(el);
        writefln("%e", stdev.getStdDev());
    }
}
