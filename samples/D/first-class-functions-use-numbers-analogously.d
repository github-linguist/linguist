import std.stdio;

auto multiplier(double a, double b)
{
    return (double c) => a * b * c;
}

void main()
{
    double x = 2.0;
    double xi = 0.5;
    double y = 4.0;
    double yi = 0.25;
    double z = x + y;
    double zi = 1.0 / (z);

    double[3] f = [x, y, z];
    double[3] r = [xi, yi, zi];

    foreach (i; 0..3)
    {
        auto mult = multiplier(f[i], r[i]);
        writefln("%f * %f * %f == %f", f[i], r[i], 1.0, mult(1));
    }
}
