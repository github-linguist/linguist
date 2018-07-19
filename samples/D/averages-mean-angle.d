import std.stdio, std.algorithm, std.complex;
import std.math: PI;

auto radians(T)(in T d) pure nothrow { return d * PI / 180; }
auto degrees(T)(in T r) pure nothrow { return r * 180 / PI; }

real meanAngle(T)(in T[] D) pure nothrow {
    immutable t = reduce!((a, d) => a + d.radians.expi)(0.complex, D);
    return (t / D.length).arg.degrees;
}

void main() {
    foreach (angles; [[350, 10], [90, 180, 270, 360], [10, 20, 30]])
        writefln("The mean angle of %s is: %.2f degrees",
                 angles, angles.meanAngle);
}
