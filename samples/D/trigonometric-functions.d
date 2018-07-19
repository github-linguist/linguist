void main() {
    import std.stdio, std.math;

    enum degrees = 45.0L;
    enum t0 = degrees * PI / 180.0L;
    writeln("Reference:  0.7071067811865475244008");
    writefln("Sine:       %.20f  %.20f", PI_4.sin, t0.sin);
    writefln("Cosine:     %.20f  %.20f", PI_4.cos, t0.cos);
    writefln("Tangent:    %.20f  %.20f", PI_4.tan, t0.tan);

    writeln;
    writeln("Reference:  0.7853981633974483096156");
    immutable real t1 = PI_4.sin.asin;
    writefln("Arcsine:    %.20f %.20f", t1, t1 * 180.0L / PI);

    immutable real t2 = PI_4.cos.acos;
    writefln("Arccosine:  %.20f %.20f", t2, t2 * 180.0L / PI);

    immutable real t3 = PI_4.tan.atan;
    writefln("Arctangent: %.20f %.20f", t3, t3 * 180.0L / PI);
}
