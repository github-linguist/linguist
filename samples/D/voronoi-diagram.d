import std.random, std.algorithm, std.range, bitmap;

struct Point { uint x, y; }

enum randomPoints = (in size_t nPoints, in size_t nx, in size_t ny) =>
    nPoints.iota
    .map!((int) => Point(uniform(0, nx), uniform(0, ny)))
    .array;

Image!RGB generateVoronoi(in Point[] pts,
                          in size_t nx, in size_t ny) /*nothrow*/ {
    // Generate a random color for each centroid.
    immutable rndRBG = (int) => RGB(cast(ubyte)uniform(0, 256),
                                    cast(ubyte)uniform(0, 256),
                                    cast(ubyte)uniform(0, 256));
    const colors = pts.length.iota.map!rndRBG.array;

    // Generate diagram by coloring pixels with color of nearest site.
    auto img = new typeof(return)(nx, ny);
    foreach (immutable x; 0 .. nx)
        foreach (immutable y; 0 .. ny) {
            immutable dCmp = (in Point a, in Point b) pure nothrow =>
                ((a.x - x) ^^ 2 + (a.y - y) ^^ 2) <
                ((b.x - x) ^^ 2 + (b.y - y) ^^ 2);
            // img[x, y] = colors[pts.reduce!(min!dCmp)];
            img[x, y] = colors[pts.length - pts.minPos!dCmp.length];
        }

    // Mark each centroid with a white dot.
    foreach (immutable p; pts)
        img[p.tupleof] = RGB.white;
    return img;
}

void main() {
    enum imageWidth = 640,
         imageHeight = 480;
    randomPoints(150, imageWidth, imageHeight)
    .generateVoronoi(imageWidth, imageHeight)
    .savePPM6("voronoi.ppm");
}
