import core.stdc.stdio, std.math, std.typecons, std.string,
       std.algorithm, std.ascii, std.array, bitmap, grayscale_image;

enum maxBrightness = 255;

alias Pixel = short;

// If normalize is true, map pixels to range 0...maxBrightness.
void convolution(bool normalize)(in Pixel[] inp, Pixel[] outp,
                                 in float[] kernel,
                                 in int nx, in int ny, in int kn)
pure nothrow in {
    assert(kernel.length == kn ^^ 2);
    assert(kn % 2 == 1);
    assert(nx > kn && ny > kn);
    assert(inp.length == outp.length);
} body {
    //immutable int kn = sqrti(kernel.length);
    immutable int khalf = kn / 2;

    static if (normalize) {
        float pMin = float.max, pMax = -float.max;

        foreach (immutable m; khalf .. nx - khalf) {
            foreach (immutable n; khalf .. ny - khalf) {
                float pixel = 0.0;
                size_t c;
                foreach (immutable j; -khalf .. khalf + 1) {
                    foreach (immutable i; -khalf .. khalf + 1) {
                        pixel += inp[(n - j) * nx + m - i] * kernel[c];
                        c++;
                    }
                }

                if (pixel < pMin) pMin = pixel;
                if (pixel > pMax) pMax = pixel;
            }
        }
    }

    foreach (immutable m; khalf .. nx - khalf) {
        foreach (immutable n; khalf .. ny - khalf) {
            float pixel = 0.0;
            size_t c;
            foreach (immutable j; -khalf .. khalf + 1) {
                foreach (immutable i; -khalf .. khalf + 1) {
                    pixel += inp[(n - j) * nx + m - i] * kernel[c];
                    c++;
                }
            }

            static if (normalize)
                pixel = maxBrightness * (pixel - pMin) / (pMax - pMin);
            outp[n * nx + m] = cast(Pixel)pixel;
        }
    }
}


void gaussianFilter(in Pixel[] inp, Pixel[] outp,
                    in int nx, in int ny, in float sigma)
pure nothrow in {
    assert(inp.length == outp.length);
} body {
    immutable int n = 2 * cast(int)(2 * sigma) + 3;
    immutable float mean = floor(n / 2.0);
    auto kernel = new float[n * n];

    debug fprintf(stderr,
                  "gaussianFilter: kernel size %d, sigma=%g\n",
                  n, sigma);

    size_t c;
    foreach (immutable i; 0 .. n) {
        foreach (immutable j; 0 .. n) {
            kernel[c] = exp(-0.5 * (((i - mean) / sigma) ^^ 2 +
                                    ((j - mean) / sigma) ^^ 2))
                        / (2 * PI * sigma * sigma);
            c++;
        }
    }

    convolution!true(inp, outp, kernel, nx, ny, n);
}


Image!Pixel cannyEdgeDetection(in Image!Pixel inp,
                               in int tMin, in int tMax,
                               in float sigma)
pure nothrow in {
    assert(inp !is null);
} body {
    immutable int nx = inp.nx;
    immutable int ny = inp.ny;
    auto outp = new Pixel[nx * ny];

    gaussianFilter(inp.image, outp, nx, ny, sigma);

    static immutable float[] Gx = [-1, 0, 1,
                                   -2, 0, 2,
                                   -1, 0, 1];
    auto after_Gx = new Pixel[nx * ny];
    convolution!false(outp, after_Gx, Gx, nx, ny, 3);

    static immutable float[] Gy = [ 1, 2, 1,
                                    0, 0, 0,
                                   -1,-2,-1];
    auto after_Gy = new Pixel[nx * ny];
    convolution!false(outp, after_Gy, Gy, nx, ny, 3);

    auto G = new Pixel[nx * ny];
    foreach (i; 1 .. nx - 1)
        foreach (j; 1 .. ny - 1) {
            immutable size_t c = i + nx * j;
            G[c] = cast(Pixel)hypot(after_Gx[c], after_Gy[c]);
        }

    // Non-maximum suppression, straightforward implementation.
    auto nms = new Pixel[nx * ny];
    foreach (immutable i; 1 .. nx - 1)
        foreach (immutable j; 1 .. ny - 1) {
            immutable int c = i + nx * j,
                          nn = c - nx,
                          ss = c + nx,
                          ww = c + 1,
                          ee = c - 1,
                          nw = nn + 1,
                          ne = nn - 1,
                          sw = ss + 1,
                          se = ss - 1;

            immutable aux = atan2(double(after_Gy[c]),
                                  double(after_Gx[c])) + PI;
            immutable float dir = float((aux % PI) / PI) * 8;

            if (((dir <= 1 || dir > 7) && G[c] > G[ee] &&
                 G[c] > G[ww]) || // 0 deg.
                ((dir > 1 && dir <= 3) && G[c] > G[nw] &&
                 G[c] > G[se]) || // 45 deg.
                ((dir > 3 && dir <= 5) && G[c] > G[nn] &&
                 G[c] > G[ss]) || // 90 deg.
                ((dir > 5 && dir <= 7) && G[c] > G[ne] &&
                 G[c] > G[sw]))   // 135 deg.
                nms[c] = G[c];
            else
                nms[c] = 0;
        }

    // Reuse array used as a stack. nx*ny/2 elements should be enough.
    int[] edges = (cast(int*)after_Gy.ptr)[0 .. after_Gy.length / 2];
    outp[] = Pixel.init;
    edges[] = 0;

    // Tracing edges with hysteresis. Non-recursive implementation.
    size_t c = 1;
    foreach (immutable j; 1 .. ny - 1) {
        foreach (immutable i; 1 .. nx - 1) {
            if (nms[c] >= tMax && outp[c] == 0) { // Trace edges.
                outp[c] = maxBrightness;
                int nedges = 1;
                edges[0] = c;

                do {
                    nedges--;
                    immutable int t = edges[nedges];

                    immutable int[8] neighbours = [
                        t - nx,      // nn
                        t + nx,      // ss
                        t + 1,       // ww
                        t - 1,       // ee
                        t - nx + 1,  // nw
                        t - nx - 1,  // ne
                        t + nx + 1,  // sw
                        t + nx - 1]; // se

                    foreach (immutable n; neighbours)
                        if (nms[n] >= tMin && outp[n] == 0) {
                            outp[n] = maxBrightness;
                            edges[nedges] = n;
                            nedges++;
                        }
                } while (nedges > 0);
            }
            c++;
        }
    }

    return Image!Pixel.fromData(outp, nx, ny);
}


void main(in string[] args) {
    immutable fileName = (args.length == 2) ? args[1] : "lena.pgm";
    Image!Pixel imIn;
    imIn = imIn.loadPGM(fileName);
    printf("Image size: %d x %d\n", imIn.nx, imIn.ny);
    imIn.cannyEdgeDetection(45, 50, 1.0f).savePGM("lena_canny.pgm");
}
