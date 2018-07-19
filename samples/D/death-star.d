import std.stdio, std.math, std.numeric, std.algorithm;

struct V3 {
    double[3] v;

    @property V3 normalize() pure nothrow const {
        immutable double len = dotProduct(v, v).sqrt;
        return [v[0] / len, v[1] / len, v[2] / len].V3;
    }

    double dot(in ref V3 y) pure nothrow const {
        immutable double d = dotProduct(v, y.v);
        return d < 0 ? -d : 0;
    }
}


const struct Sphere { double cx, cy, cz, r; }

void drawSphere(in double k, in double ambient, in V3 light) nothrow {
    /** Check if a ray (x,y, -inf).(x, y, inf) hits a sphere; if so,
    return the intersecting z values.  z1 is closer to the eye.*/
    static bool hitSphere(in ref Sphere sph,
                          in double x0, in double y0,
                          out double z1,
                          out double z2) pure nothrow {
        immutable double x = x0 - sph.cx;
        immutable double y = y0 - sph.cy;
        immutable double zsq = sph.r ^^ 2 - (x ^^ 2 + y ^^ 2);
        if (zsq < 0)
            return false;
        immutable double szsq = zsq.sqrt;
        z1 = sph.cz - szsq;
        z2 = sph.cz + szsq;
        return true;
    }

    immutable shades = ".:!*oe&#%@";
    // Positive and negative spheres.
    immutable pos = Sphere(20, 20, 0, 20);
    immutable neg = Sphere(1, 1, -6, 20);

    foreach (immutable int i; cast(int)floor(pos.cy - pos.r) ..
                              cast(int)ceil(pos.cy + pos.r) + 1) {
        immutable double y = i + 0.5;
    JLOOP:
        foreach (int j; cast(int)floor(pos.cx - 2 * pos.r) ..
                        cast(int)ceil(pos.cx + 2 * pos.r) + 1) {
            immutable double x = (j - pos.cx) / 2.0 + 0.5 + pos.cx;

            enum Hit { background, posSphere, negSphere }

            double zb1, zs2;
            immutable Hit hitResult = {
                double zb2, zs1;

                if (!hitSphere(pos, x, y, zb1, zb2)) {
                    // Ray lands in blank space, draw bg.
                    return Hit.background;
                } else if (!hitSphere(neg, x, y, zs1, zs2)) {
                    // Ray hits pos sphere but not neg one,
                    // draw pos sphere surface.
                    return Hit.posSphere;
                } else if (zs1 > zb1) {
                    // ray hits both, but pos front surface is closer.
                    return Hit.posSphere;
                } else if (zs2 > zb2) {
                    // pos sphere surface is inside neg sphere,
                    // show bg.
                    return Hit.background;
                } else if (zs2 > zb1) {
                    // Back surface on neg sphere is inside pos
                    // sphere, the only place where neg sphere
                    // surface will be shown.
                    return Hit.negSphere;
                } else {
                    return Hit.posSphere;
                }
            }();

            V3 vec_;
            final switch (hitResult) {
                case Hit.background:
                    ' '.putchar;
                    continue JLOOP;
                case Hit.posSphere:
                    vec_ = [x - pos.cx, y - pos.cy, zb1 - pos.cz].V3;
                    break;
                case Hit.negSphere:
                    vec_ = [neg.cx - x, neg.cy - y, neg.cz - zs2].V3;
                    break;
            }
            immutable nvec = vec_.normalize;

            immutable double b = light.dot(nvec) ^^ k + ambient;
            immutable intensity = cast(int)((1 - b) * shades.length);
            immutable normInt = min(shades.length, max(0, intensity));
            shades[normInt].putchar;
        }

        '\n'.putchar;
    }
}


void main() {
    immutable light = [-50, 30, 50].V3.normalize;
    drawSphere(2, 0.5, light);
}
