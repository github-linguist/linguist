import grayscale_image;

Image!Color medianFilter(uint radius=10, Color)(in Image!Color img)
pure nothrow if (radius > 0) {
    alias Hist = uint[256];

    static ubyte median(uint no)(in ref Hist cumulative) pure nothrow {
        size_t localSum = 0;
        foreach (immutable ubyte k, immutable v; cumulative)
            if (v) {
                localSum += v;
                if (localSum > no / 2)
                    return k;
            }
        return 0;
    }

    // Copy image borders in the result image.
    auto result = new Image!Color(img.nx, img.ny);
    foreach (immutable y; 0 .. img.ny)
        foreach (immutable x; 0 .. img.nx)
            if (x < radius || x > img.nx - radius ||
                y < radius || y > img.ny - radius)
                result[x, y] = img[x, y];

    enum edge = 2 * radius + 1;
    auto hCol = new Hist[img.nx];

    // Create histogram columns.
    foreach (immutable y; 0 .. edge - 1)
        foreach (immutable x, ref hx; hCol)
            hx[img[x, y]]++;

    foreach (immutable y; radius .. img.ny - radius) {
        // Add to each histogram column lower pixel.
        foreach (immutable x, ref hx; hCol)
            hx[img[x, y + radius]]++;

        // Calculate main Histogram using first edge-1 columns.
        Hist H;
        foreach (immutable x; 0 .. edge - 1)
            foreach (immutable k, immutable v; hCol[x])
                if (v)
                    H[k] += v;

        foreach (immutable x; radius .. img.nx - radius) {
            // Add right-most column.
            foreach (immutable k, immutable v; hCol[x + radius])
                if (v)
                    H[k] += v;

            result[x, y] = Color(median!(edge ^^ 2)(H));

            // Drop left-most column.
            foreach (immutable k, immutable v; hCol[x - radius])
                if (v)
                    H[k] -= v;
        }

        // Substract the upper pixels.
        foreach (immutable x, ref hx; hCol)
            hx[img[x, y - radius]]--;
    }

    return result;
}

version (median_filter_main) {
    void main() { // Demo.
        loadPGM!Gray(null, "lena.pgm")
        .medianFilter!10()
        .savePGM("lena_median_r10.pgm");
    }
}
