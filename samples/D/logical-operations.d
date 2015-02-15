import std.stdio;

void logic(T, U)(T lhs, U rhs) {
    writefln("'%s' is of type '%s', '%s' is of type '%s';",
             lhs, typeid(typeof(lhs)), rhs,typeid(typeof(rhs)));
    writefln("\t'%s' AND '%s' is %s, ", lhs, rhs, lhs && rhs);
    writefln("\t'%s' OR '%s' is %s, ", lhs, rhs, lhs || rhs);
    writefln("\tNOT '%s' is %s.\n", lhs, !lhs);
}

class C { int value; }

void main() {
    bool theTruth = true;
    bool theLie = false;
    real zeroReal = 0.0L;
    real NaN; // D initializes floating point values to NaN
    int zeroInt  = 0;
    real[] nullArr = null;
    string emptyStr = "";
    string nullStr = null;
    C someC = new C;
    C nullC = null;

    // Note: Struct is value type in D, but composite
    //  so no default bool equivalent.

    logic(theTruth, theLie);
    logic(zeroReal, NaN);
    logic(zeroInt, nullArr);
    logic(nullStr, emptyStr);
    logic(someC, nullC);
}
