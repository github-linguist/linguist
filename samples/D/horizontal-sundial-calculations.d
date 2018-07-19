import std.stdio, std.math, std.conv, std.string;

double radians(in double x) pure nothrow { return x * (PI / 180); }
double degrees(in double x) pure nothrow { return x / (PI / 180); }

T input(T)(in string msg) {
    msg.write;
    return readln.strip.to!T;
}

void main() {
    immutable lat = input!double("Enter latitude       => ");
    immutable lng = input!double("Enter longitude      => ");
    immutable lme = input!double("Enter legal meridian => ");
    writeln;

    double slat = lat.radians.sin;
    writefln("    sine of latitude:   %.3f", slat);
    writefln("    diff longitude:     %.3f", lng - lme);
    writeln;
    "Hour, sun hour angle, dial hour line angle from 6am to 6pm".writeln;

    foreach (immutable h; -6 .. 7) {
        immutable double hra = 15 * h - (lng - lme);
        immutable double hla = atan(slat * hra.radians.tan).degrees;
        writefln("HR=%3d; HRA=%7.3f; HLA=%7.3f", h, hra, hla);
    }
}
