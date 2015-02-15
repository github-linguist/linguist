import std.string, std.math, std.algorithm, grayscale_image;

struct ConvolutionFilter {
    double[][] kernel;
    double divisor, offset_;
    string name;
}


Image!Color convolve(Color)(in Image!Color im,
                            in ConvolutionFilter filter)
pure nothrow in {
    assert(im !is null);
    assert(!isnan(filter.divisor) && !isnan(filter.offset_));
    assert(filter.divisor != 0);
    assert(filter.kernel.length > 0 && filter.kernel[0].length > 0);
    foreach (const row; filter.kernel) // Is rectangular.
        assert(row.length == filter.kernel[0].length);
    assert(filter.kernel.length % 2 == 1); // Odd sized kernel.
    assert(filter.kernel[0].length % 2 == 1);
    assert(im.ny >= filter.kernel.length);
    assert(im.nx >= filter.kernel[0].length);
} out(result) {
    assert(result !is null);
    assert(result.nx == im.nx && result.ny == im.ny);
} body {
    immutable knx2 = filter.kernel[0].length / 2;
    immutable kny2 = filter.kernel.length / 2;
    auto io = new Image!Color(im.nx, im.ny);

    static if (is(Color == RGB))
        alias CT = typeof(Color.r); // Component type.
    else static if (is(typeof(Color.c)))
        alias CT = typeof(Color.c);
    else
        alias CT = Color;

    foreach (immutable y; kny2 .. im.ny - kny2) {
        foreach (immutable x; knx2 .. im.nx - knx2) {
            static if (is(Color == RGB))
                double[3] total = 0.0;
            else
                double total = 0.0;

            foreach (immutable sy, const kRow; filter.kernel) {
                foreach (immutable sx, immutable k; kRow) {
                    immutable p = im[x + sx - knx2, y + sy - kny2];
                    static if (is(Color == RGB)) {
                        total[0] += p.r * k;
                        total[1] += p.g * k;
                        total[2] += p.b * k;
                    } else {
                        total += p * k;
                    }
                }
            }

            immutable D = filter.divisor;
            immutable O = filter.offset_ * CT.max;
            static if (is(Color == RGB)) {
                io[x, y] = Color(
                    cast(CT)min(max(total[0]/ D + O, CT.min), CT.max),
                    cast(CT)min(max(total[1]/ D + O, CT.min), CT.max),
                    cast(CT)min(max(total[2]/ D + O, CT.min), CT.max));
            } else static if (is(typeof(Color.c))) {
                io[x, y] = Color(
                    cast(CT)min(max(total / D + O, CT.min), CT.max));
            } else {
                // If Color doesn't have a 'c' field, then Color is
                // assumed to be a built-in type.
                io[x, y] =
                    cast(CT)min(max(total / D + O, CT.min), CT.max);
            }
        }
    }

    return io;
}


void main() {
    immutable ConvolutionFilter[] filters = [
        {[[-2.0, -1.0, 0.0],
          [-1.0,  1.0, 1.0],
          [ 0.0,  1.0, 2.0]], divisor:1.0, offset_:0.0, name:"Emboss"},

        {[[-1.0, -1.0, -1.0],
          [-1.0,  9.0, -1.0],
          [-1.0, -1.0, -1.0]], divisor:1.0, 0.0, "Sharpen"},

        {[[-1.0, -2.0, -1.0],
          [ 0.0,  0.0,  0.0],
          [ 1.0,  2.0,  1.0]], divisor:1.0, 0.5, "Sobel_emboss"},

        {[[1.0, 1.0, 1.0],
          [1.0, 1.0, 1.0],
          [1.0, 1.0, 1.0]], divisor:9.0, 0.0, "Box_blur"},

        {[[1,  4,  7,  4, 1],
          [4, 16, 26, 16, 4],
          [7, 26, 41, 26, 7],
          [4, 16, 26, 16, 4],
          [1,  4,  7,  4, 1]], divisor:273, 0.0, "Gaussian_blur"}];

    Image!RGB im;
    im.loadPPM6("Lenna100.ppm");

    foreach (immutable filter; filters)
        im.convolve(filter)
        .savePPM6(format("lenna_%s.ppm", filter.name));

    const img = im.rgb2grayImage();
    foreach (immutable filter; filters)
        img.convolve(filter)
        .savePGM(format("lenna_gray_%s.ppm", filter.name));
}
