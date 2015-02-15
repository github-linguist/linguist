pragma(lib, "user32.lib");

import std.stdio, std.c.windows.windows;

extern(Windows) UINT GetDoubleClickTime();

void main() {
    writeln(GetDoubleClickTime());
}
