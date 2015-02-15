void if2(T1, T2, T3, T4)(in bool c1, in bool c2,
                         lazy T1 first,
                         lazy T2 both,
                         lazy T3 second,
                         lazy T4 none) {
    if (c1) {
        if (c2)
            both;
        else
            first;
    } else {
        if (c2)
            second;
        else
            none;
    }
}

void test(in bool a, in bool b) {
    import std.stdio;
    if2(a, b, writeln("first"),
              writeln("both"),
              writeln("second"),
              writeln("none"));
}

void main() {
    test(1 < 2, 3 > 4);
    test(1 < 2, 1 < 2);
    test(3 > 4, 3 > 4);
    test(3 > 4, 1 < 2);
}
