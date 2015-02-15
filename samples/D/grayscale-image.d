module grayscale_image;

import core.stdc.stdio, std.array, std.algorithm, std.string, std.ascii;
public import bitmap;

struct Gray {
    ubyte c;
    enum black = typeof(this)(0);
    enum white = typeof(this)(255);
    alias c this;
}


Image!Color loadPGM(Color)(Image!Color img, in string fileName) {
    static int readNum(FILE* f) nothrow {
        int n;
        while (!fscanf(f, "%d ", &n)) {
            if ((n = fgetc(f)) == '#') {
                while ((n = fgetc(f)) != '\n')
                    if (n == EOF)
                        return 0;
            } else
                return 0;
        }
        return n;
    }

    scope(exit) if (fin) fclose(fin);
    if (img is null)
        img = new Image!Color();

    auto fin = fopen(fileName.toStringz(), "rb");
    if (!fin)
        throw new Exception("Can't open input file.");

    if (fgetc(fin) != 'P' ||
        fgetc(fin) != '5' ||
        !isWhite(fgetc(fin)))
        throw new Exception("Not a PGM (PPM P5) image.");

    immutable int nc = readNum(fin);
    immutable int nr = readNum(fin);
    immutable int maxVal = readNum(fin);
    if (nc <= 0 || nr <= 0 || maxVal <= 0)
        throw new Exception("Wrong input image sizes.");
    img.allocate(nc, nr);
    auto pix = new ubyte[img.image.length];

    immutable count = fread(pix.ptr, 1, nc * nr, fin);
    if (count != nc * nr)
        throw new Exception("Wrong number of items read.");

    pix.copy(img.image);
    return img;
}


void savePGM(Color)(in Image!Color img, in string fileName)
in {
    assert(img !is null);
    assert(!fileName.empty);
    assert(img.nx > 0 && img.ny > 0 &&
           img.image.length == img.nx * img.ny,
           "Wrong image.");
} body {
    auto fout = fopen(fileName.toStringz(), "wb");
    if (fout == null)
        throw new Exception("File can't be opened.");
    fprintf(fout, "P5\n%d %d\n255\n", img.nx, img.ny);
    auto pix = new ubyte[img.image.length];
    foreach (i, ref p; pix)
        p = cast(typeof(pix[0]))img.image[i];
    immutable count = fwrite(pix.ptr, ubyte.sizeof,
                             img.nx * img.ny, fout);
    if (count != img.nx * img.ny)
        new Exception("Wrong number of items written.");
    fclose(fout);
}


Gray lumCIE(in RGB c) pure nothrow {
    return Gray(cast(ubyte)(0.2126 * c.r +
                            0.7152 * c.g +
                            0.0722 * c.b + 0.5));
}

Gray lumAVG(in RGB c) pure nothrow {
    return Gray(cast(ubyte)(0.3333 * c.r +
                            0.3333 * c.g +
                            0.3333 * c.b + 0.5));
}

Image!Gray rgb2grayImage(alias Conv=lumCIE)(in Image!RGB im) {
    auto result = new typeof(return)(im.nx, im.ny);
    foreach (i, immutable rgb; im.image)
        result.image[i] = Conv(rgb);
    return result;
}

Image!RGB gray2rgbImage(in Image!Gray im) {
    auto result = new typeof(return)(im.nx, im.ny);
    foreach (i, immutable gr; im.image)
        result.image[i] = RGB(gr, gr, gr);
    return result;
}

version (grayscale_image_main) {
    void main() {
        auto im1 = new Image!Gray;
        im1.loadPGM("lena.pgm");
        gray2rgbImage(im1).savePPM6("lena_rgb.ppm");

        auto img2 = new Image!RGB;
        img2.loadPPM6("quantum_frog.ppm");
        img2.rgb2grayImage.savePGM("quantum_frog_grey.pgm");
    }
}
