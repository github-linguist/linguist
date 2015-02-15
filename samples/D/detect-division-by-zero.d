import std.stdio, std.string, std.math, std.traits;

string divCheck(T)(in T numer, in T denom)
if (isIntegral!T || isFloatingPoint!T) {
    Unqual!(typeof(numer / denom)) result;
    string msg;

    static if (isIntegral!T) {
        try {
            result = numer / denom;
        } catch(Error e) {
            msg = "| " ~ e.msg ~ " (by Error)";
            result = T.max;
        }
    } else { // Floating Point Type.
        result = numer / denom;
        if (numer.isNormal && result.isInfinity) {
            msg = "| Division by Zero";
        } else if (result != 0 && !result.isNormal) {
            if (numer.isNaN)
                msg = "| NaN numerator";
            else if (denom.isNaN)
                msg = "| NaN denominator";
            else if (numer.isInfinity)
                msg = "| Inf numerator";
            else
                msg = "| NaN (Zero Division by Zero)";
        }
    }

    return format("%5s %s", format("%1.1g", real(result)), msg);
}

void main() {
    writeln("Division with check:");
    writefln("int     1/ 0:   %s", divCheck(1, 0));
    writefln("ubyte   1/ 0:   %s", divCheck(ubyte(1), ubyte(0)));
    writefln("real    1/ 0:   %s", divCheck(1.0L, 0.0L));
    writefln("real   -1/ 0:   %s", divCheck(-1.0L, 0.0L));
    writefln("real    0/ 0:   %s", divCheck(0.0L, 0.0L));
    writeln;
    writefln("real   -4/-2:   %s", divCheck(-4.0L,-2.0L));
    writefln("real    2/-inf: %s", divCheck(2.0L, -real.infinity));
    writeln;
    writefln("real -inf/-2:   %s", divCheck(-real.infinity, -2.0L));
    writefln("real +inf/-2:   %s", divCheck(real.infinity, -2.0L));
    writefln("real  nan/-2:   %s", divCheck(real.nan, -2.0L));
    writefln("real   -2/ nan: %s", divCheck(-2.0L, real.nan));
    writefln("real  nan/ 0:   %s", divCheck(real.nan, 0.0L));
    writefln("real  inf/ inf: %s",
             divCheck(real.infinity, real.infinity));
    writefln("real  nan/ nan: %s", divCheck(real.nan, real.nan));
}
