import std.stdio;

/// Throw Exceptions
/// Stack traces are generated compiling with the -g switch.
void test1() {
  throw new Exception("Sample Exception");
}

/// Catch Exceptions
void test2() {
    try {
        test1();
    } catch (Exception ex) {
        writeln(ex);
        throw ex; // rethrow
    }
}

/// Ways to implement finally
void test3() {
    try test2();
    finally writeln("test3 finally");
}

/// Or also with scope guards
void test4() {
    scope(exit) writeln("Test4 done");
    scope(failure) writeln("Test4 exited by exception");
    scope(success) writeln("Test4 exited by return or function end");
    test2();
}

void main() {
    test4();
}
