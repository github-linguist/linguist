import std.math, std.algorithm, grayscale_image;

/// Plots anti-aliased line by Xiaolin Wu's line algorithm.
void aaLine(Color)(ref Image!Color img,
                   double x1, double y1,
                   double x2, double y2,
                   in Color color) /*pure*/ nothrow {
    // Straight translation of Wikipedia pseudocode.
    static double round(in double x) pure nothrow {
        return floor(x + 0.5);
    }

    static double fpart(in double x) pure nothrow {
        return x - x.floor;
    }

    static double rfpart(in double x) pure nothrow {
        return 1 - fpart(x);
    }

    auto dx = x2 - x1;
    auto dy = y2 - y1;
    immutable ax = dx.abs;
    immutable ay = dy.abs;

    static Color mixColors(in Color c1, in Color c2, in double p)
    pure nothrow {
        static if (is(Color == RGB))
            return Color(cast(ubyte)(c1.r * p + c2.r * (1 - p)),
                         cast(ubyte)(c1.g * p + c2.g * (1 - p)),
                         cast(ubyte)(c1.b * p + c2.b * (1 - p)));
        else
            // This doesn't work for every kind of Color.
            return Color(cast(ubyte)(c1 * p + c2 * (1 - p)));
    }

    // Plot function set here to handle the two cases of slope.
    void delegate(ref Image!Color img, in int, in int, in double)
    pure nothrow plot;

    if (ax < ay) {
        swap(x1, y1);
        swap(x2, y2);
        swap(dx, dy);
        //plot = (img, x, y, p) {
        plot = (ref Image!Color img, x, y, p) {
            assert(p >= 0.0 && p <= 1.0);
            img[y, x] = mixColors(color, img[y, x], p);
        };
    } else {
        //plot = (img, x, y, p) {
        plot = (ref Image!Color img, x, y, p) {
            assert(p >= 0.0 && p <= 1.0);
            img[x, y] = mixColors(color, img[x, y], p);
        };
    }

    if (x2 < x1) {
        swap(x1, x2);
        swap(y1, y2);
    }
    immutable gradient = dy / dx;

    // Handle first endpoint.
    auto xEnd = x1.round; // Not pure.
    auto yEnd = y1 + gradient * (xEnd - x1);
    auto xGap = rfpart(x1 + 0.5);
    // This will be used in the main loop.
    immutable xpxl1 = cast(int)xEnd;
    immutable ypxl1 = cast(int)yEnd.floor;
    plot(img, xpxl1, ypxl1, rfpart(yEnd) * xGap);
    plot(img, xpxl1, ypxl1 + 1, fpart(yEnd) * xGap);
    // First y-intersection for the main loop.
    auto yInter = yEnd + gradient;

    // Handle second endpoint.
    xEnd = x2.round;
    yEnd = y2 + gradient * (xEnd - x2);
    xGap = fpart(x2 + 0.5);
    // This will be used in the main loop.
    immutable xpxl2 = cast(int)xEnd;
    immutable ypxl2 = cast(int)yEnd.floor;
    plot(img, xpxl2, ypxl2, rfpart(yEnd) * xGap);
    plot(img, xpxl2, ypxl2 + 1, fpart(yEnd) * xGap);

    // Main loop.
    foreach (immutable x; xpxl1 + 1 .. xpxl2) {
        plot(img, x, cast(int)yInter.floor, rfpart(yInter));
        plot(img, x, cast(int)yInter.floor + 1, fpart(yInter));
        yInter += gradient;
    }
}

void main() {
    auto im1 = new Image!Gray(400, 300);
    im1.clear(Gray.white);
    im1.aaLine(7.4, 12.3, 307, 122.5, Gray.black);
    im1.aaLine(177.4, 12.3, 127, 222.5, Gray.black);
    im1.savePGM("xiaolin_lines1.pgm");

    auto im2 = new Image!RGB(400, 300);
    im2.clear(RGB(0, 255, 0));
    immutable red = RGB(255, 0, 0);
    im2.aaLine(7.4, 12.3, 307, 122.5, red);
    im2.aaLine(177.4, 12.3, 127, 222.5, red);
    im2.savePPM6("xiaolin_lines2.ppm");
}
