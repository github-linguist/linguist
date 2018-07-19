import std.math, grayscale_image;

Image!Gray houghTransform(in Image!Gray im,
                          in size_t hx=460, in size_t hy=360)
pure nothrow in {
    assert(im !is null);
    assert(hx > 0 && hy > 0);
    assert((hy & 1) == 0, "hy argument must be even.");
} body {
    auto result = new Image!Gray(hx, hy);
    result.clear(Gray.white);

    immutable double rMax = hypot(im.nx, im.ny);
    immutable double dr = rMax / (hy / 2.0);
    immutable double dTh = PI / hx;

    foreach (immutable y; 0 .. im.ny) {
        foreach (immutable x; 0 .. im.nx) {
            if (im[x, y] == Gray.white)
                continue;
            foreach (immutable iTh; 0 .. hx) {
                immutable double th = dTh * iTh;
                immutable double r = x * cos(th) + y * sin(th);
                immutable iry = hy / 2 - cast(int)floor(r / dr + 0.5);
                if (result[iTh, iry] > Gray(0))
                    result[iTh, iry]--;
            }
        }
    }
    return result;
}

void main() {
    (new Image!RGB)
    .loadPPM6("Pentagon.ppm")
    .rgb2grayImage()
    .houghTransform()
    .savePGM("Pentagon_hough.pgm");
}
