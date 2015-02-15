double kelvinToCelsius(in double k) pure nothrow @safe {
    return k - 273.15;
}

double kelvinToFahrenheit(in double k) pure nothrow @safe {
    return k * 1.8 - 459.67;
}

double kelvinToRankine(in double k) pure nothrow @safe {
    return k * 1.8;
}

unittest {
    import std.math: approxEqual;
    assert(approxEqual(kelvinToCelsius(21.0), -252.15));
    assert(approxEqual(kelvinToFahrenheit(21.0), -421.87));
    assert(approxEqual(kelvinToRankine(21.0), 37.8));
}

void main(string[] args) {
    import std.stdio, std.conv, std.string;

    if (args.length == 2 && isNumeric(args[1])) {
        immutable kelvin = to!double(args[1]);
        if (kelvin >= 0) {
            writefln("K  %2.2f", kelvin);
            writefln("C  %2.2f", kelvinToCelsius(kelvin));
            writefln("F  %2.2f", kelvinToFahrenheit(kelvin));
            writefln("R  %2.2f", kelvinToRankine(kelvin));
        } else
            writefln("%2.2f K is below absolute zero", kelvin);
    }
}
