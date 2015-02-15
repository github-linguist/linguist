import std.stdio;

void main()
{
    auto lp = File("/dev/lp0", "w");
    lp.writeln("Hello World!");
}
