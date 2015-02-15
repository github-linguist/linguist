import std.stdio, std.math, std.algorithm, std.numeric;

alias double[3] V3;
V3 light = [30, 30, -50];

void normalize(ref V3 v) pure {
    v[] /= dotProduct(v, v) ^^ 0.5;
}

double dot(in ref V3 x, in ref V3 y) pure nothrow {
    immutable double d = dotProduct(x, y);
    return d < 0 ? -d : 0;
}

void drawSphere(in double R, in double k, in double ambient) {
    enum shades = ".:!*oe&#%@";
    foreach (int i; cast(int)floor(-R) .. cast(int)ceil(R) + 1) {
        immutable double x = i + 0.5;
        foreach (int j; cast(int)floor(-2*R)..cast(int)ceil(2*R)+1){
            immutable double y = j / 2. + 0.5;
            if (x ^^ 2 + y ^^ 2 <= R ^^ 2) {
                V3 vec = [x, y, (R^^2 - x^^2 - y^^2) ^^ 0.5];
                vec.normalize();
                immutable double b = dot(light, vec) ^^ k + ambient;
                int intensity = cast(int)((1-b) * (shades.length-1));
                intensity = min(shades.length-1, max(intensity, 0));
                putchar(shades[intensity]);
            } else
                putchar(' ');
        }
        putchar('\n');
    }
}

void main() {
    light.normalize();
    drawSphere(20, 4, 0.1);
    drawSphere(10, 2, 0.4);
}
