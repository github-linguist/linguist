import core.stdc.stdio, std.stdio, std.algorithm, std.typecons,
       std.math, std.range, std.conv, std.string, bitmap;

struct Col { float r, g, b; }
alias Cluster = Tuple!(Col, float, Col, Col[]);
enum Axis { R, G, B }

enum round = (in float x) pure nothrow => cast(int)floor(x + 0.5);

enum roundRGB = (in Col c) pure nothrow => RGB(cast(ubyte)round(c.r),
                                               cast(ubyte)round(c.g),
                                               cast(ubyte)round(c.b));

enum addRGB = (in Col c1, in Col c2) pure nothrow =>
    Col(c1.r + c2.r, c1.g + c2.g, c1.b + c2.b);

Col meanRGB(in Col[] pxList) pure nothrow {
    immutable tot = reduce!addRGB(Col(0, 0, 0), pxList);
    immutable n = pxList.length;
    return Col(tot.r / n, tot.g / n, tot.b / n);
}

enum minC = (in Col c1, in Col c2) pure nothrow =>
    Col(min(c1.r, c2.r), min(c1.g, c2.g), min(c1.b, c2.b));

enum maxC = (in Col c1, in Col c2) pure nothrow =>
    Col(max(c1.r, c2.r), max(c1.g, c2.g), max(c1.b, c2.b));

Tuple!(Col, Col) extrems(in Col[] lst) pure nothrow {
    enum FI = float.infinity;
    auto mmRGB = typeof(return)(Col(FI, FI, FI), Col(-FI, -FI, -FI));
    return reduce!(minC, maxC)(mmRGB, lst);
}

Tuple!(float, Col) volumeAndDims(in Col[] lst) pure nothrow {
    immutable e = lst.extrems;
    immutable r = Col(e[1].r - e[0].r,
                      e[1].g - e[0].g,
                      e[1].b - e[0].b);
    return typeof(return)(r.r * r.g * r.b, r);
}

Cluster makeCluster(Col[] pixelList) pure nothrow {
    immutable vol_dims = pixelList.volumeAndDims;
    immutable int len = pixelList.length;
    return typeof(return)(pixelList.meanRGB,
                          len * vol_dims[0],
                          vol_dims[1],
                          pixelList);
}

enum fCmp = (in float a, in float b) pure nothrow =>
    (a > b) ? 1 : (a < b ? -1 : 0);

Axis largestAxis(in Col c) pure nothrow {
    immutable int r1 = fCmp(c.r, c.g);
    immutable int r2 = fCmp(c.r, c.b);
    if (r1 ==  1 && r2 ==  1) return Axis.R;
    if (r1 == -1 && r2 ==  1) return Axis.G;
    if (r1 ==  1 && r2 == -1) return Axis.B;
    return (fCmp(c.g, c.b) == 1) ? Axis.G : Axis.B;
}

Tuple!(Cluster, Cluster) subdivide(in Col c, in float nVolProd,
                                   in Col vol, Col[] pixels)
pure nothrow {
    bool delegate(immutable Col) pure nothrow partFunc;
    final switch (largestAxis(vol)) {
        case Axis.R: partFunc = c1 => c1.r < c.r; break;
        case Axis.G: partFunc = c1 => c1.g < c.g; break;
        case Axis.B: partFunc = c1 => c1.b < c.b; break;
    }
    auto px2 = pixels.partition!partFunc;
    auto px1 = pixels[0 .. $ - px2.length];
    return typeof(return)(px1.makeCluster, px2.makeCluster);
}

uint RGB2uint(in RGB c) pure nothrow {
    return c.r | (c.g << 8) | (c.b << 16);
}

enum uintToRGB = (in uint c) pure nothrow =>
    RGB(c & 0xFF, (c >> 8) & 0xFF, (c >> 16) & 0xFF);

Image!RGB colorQuantize(in Image!RGB img, in int n) pure nothrow {
    immutable width = img.nx;
    immutable height = img.ny;

    auto cols = new Col[width * height];
    foreach (immutable i, ref c; img.image)
        cols[i] = Col(c.tupleof);

    immutable dumb = Col(0, 0, 0);
    Cluster unused= Cluster(dumb, -float.infinity, dumb, (Col[]).init);

    auto clusters = [cols.makeCluster];
    while (clusters.length < n) {
        // Cluster cl = clusters.reduce!(max!q{ a[1] })(unused);
        Cluster cl = reduce!((c1, c2) => c1[1] > c2[1] ? c1 : c2)
                            (unused, clusters);
        clusters = [cl[].subdivide[]] ~
            clusters.remove!(c => c == cl, SwapStrategy.unstable);
    }

    uint[uint] pixMap; // Faster than RGB[RGB].
    ubyte[4] u4a, u4b;
    foreach (const cluster; clusters) {
        immutable ubyteMean = cluster[0].roundRGB.RGB2uint;
        foreach (immutable col; cluster[3])
            pixMap[col.roundRGB.RGB2uint] = ubyteMean;
    }

    auto result = new Image!RGB;
    result.allocate(height, width);

    foreach (immutable i, immutable p; img.image) {
        immutable u3a = p.tupleof.RGB;
        result.image[i] = pixMap[RGB2uint(u3a)].uintToRGB;
    }

    return result;
}

void main(in string[] args) {
    string fileName;
    int nCols;
    switch (args.length) {
        case 1:
            fileName = "quantum_frog.ppm";
            nCols = 16;
            break;
        case 3:
            fileName = args[1];
            nCols = args[2].to!int;
            break;
        default:
            "Usage: color_quantization image.ppm ncolors".writeln;
            return;
    }

    auto im = new Image!RGB;
    im.loadPPM6(fileName);
    const imq = colorQuantize(im, nCols);
    imq.savePPM6("quantum_frog_quantized.ppm");
}
