import std.stdio, std.regex, std.conv, std.string, std.range,
       arithmetic_rational;

struct Pair { int x; Rational r; }

Pair[][] parseEquations(in string text) /*pure nothrow*/ {
    auto r = regex(r"\s*(?P<sign>[+-])?\s*(?:(?P<mul>\d+)\s*\*)?\s*" ~
                   r"arctan\((?P<num>\d+)/(?P<denom>\d+)\)");
    Pair[][] machins;
    foreach (const line; text.splitLines) {
        Pair[] formula;
        foreach (/*const*/ part; line.split("=")[1].matchAll(r)) {
            immutable mul = part["mul"],
                      num = part["num"],
                      denom = part["denom"];
            formula ~= Pair((part["sign"] == "-" ? -1 : 1) *
                            (mul.empty ? 1 : mul.to!int),
                            Rational(num.to!int,
                                     denom.empty ? 1 : denom.to!int));
        }
        machins ~= formula;
    }
    return machins;
}


Rational tans(in Pair[] xs) pure /*nothrow*/ {
    static Rational tanEval(in int coef, in Rational f)
    pure /*nothrow*/ {
        if (coef == 1)
            return f;
        if (coef < 0)
            return -tanEval(-coef, f);
        immutable a = tanEval(coef / 2, f),
                  b = tanEval(coef - coef / 2, f);
        return (a + b) / (1 - a * b);
    }

    if (xs.length == 1)
        return tanEval(xs[0].tupleof);
    immutable a = xs[0 .. $ / 2].tans,
              b = xs[$ / 2 .. $].tans;
    return (a + b) / (1 - a * b);
}

void main() {
    immutable equationText =
"pi/4 = arctan(1/2) + arctan(1/3)
pi/4 = 2*arctan(1/3) + arctan(1/7)
pi/4 = 4*arctan(1/5) - arctan(1/239)
pi/4 = 5*arctan(1/7) + 2*arctan(3/79)
pi/4 = 5*arctan(29/278) + 7*arctan(3/79)
pi/4 = arctan(1/2) + arctan(1/5) + arctan(1/8)
pi/4 = 4*arctan(1/5) - arctan(1/70) + arctan(1/99)
pi/4 = 5*arctan(1/7) + 4*arctan(1/53) + 2*arctan(1/4443)
pi/4 = 6*arctan(1/8) + 2*arctan(1/57) + arctan(1/239)
pi/4 = 8*arctan(1/10) - arctan(1/239) - 4*arctan(1/515)
pi/4 = 12*arctan(1/18) + 8*arctan(1/57) - 5*arctan(1/239)
pi/4 = 16*arctan(1/21) + 3*arctan(1/239) + 4*arctan(3/1042)
pi/4 = 22*arctan(1/28) + 2*arctan(1/443) - 5*arctan(1/1393) - 10*arctan(1/11018)
pi/4 = 22*arctan(1/38) + 17*arctan(7/601) + 10*arctan(7/8149)
pi/4 = 44*arctan(1/57) + 7*arctan(1/239) - 12*arctan(1/682) + 24*arctan(1/12943)
pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12943)
pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12944)";

    const machins = equationText.parseEquations;
    foreach (const machin, const eqn; machins.zip(equationText.splitLines)) {
        immutable ans = machin.tans;
        writefln("%5s: %s", ans == 1 ? "OK" : "ERROR", eqn);
    }
}
