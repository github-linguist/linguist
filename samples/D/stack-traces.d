import std.stdio, core.runtime;

void inner() { defaultTraceHandler.writeln; }
void middle() { inner; }
void outer() { middle; }

void main() {
    outer;
    "After the stack trace.".writeln;
}
