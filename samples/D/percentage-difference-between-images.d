import std.stdio, std.exception, std.range, std.math, bitmap;

void main() {
    Image!RGB i1, i2;
    i1 = i1.loadPPM6("Lenna50.ppm");
    i2 = i2.loadPPM6("Lenna100.ppm");

    enforce(i1.nx == i2.nx && i1.ny == i2.ny, "Different sizes.");

    real dif = 0.0;
    foreach (p, q; zip(i1.image, i2.image))
        dif += abs(p.r - q.r) + abs(p.g - q.g) + abs(p.b - q.b);

    immutable nData = i1.nx * i1.ny * 3;
    writeln("Difference (percentage): ",
            (dif / 255.0 * 100) / nData);
}
