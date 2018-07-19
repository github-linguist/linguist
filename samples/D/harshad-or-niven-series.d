void main() {
    import std.stdio, std.algorithm, std.range, std.conv;

    enum digSum = (int n) => n.text.map!(d => d - '0').sum;
    enum harshads = iota(1, int.max).filter!(n => n % digSum(n) == 0);

    harshads.take(20).writeln;
    harshads.filter!(h => h > 1000).front.writeln;
}
