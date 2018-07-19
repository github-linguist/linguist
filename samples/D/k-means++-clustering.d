import std.stdio, std.math, std.random, std.typecons, std.algorithm;

// On Windows this uses the printf from the Microsoft C runtime,
// that doesn't handle real type and some of the C99 format
// specifiers, but it's faster for blunk printing.
extern(C) nothrow int printf(const char*, ...);

struct Point {
    immutable double x, y; // Or float.
    size_t cluster;
}

Point[] generatePoints(in size_t nPoints,
                       in double radius,
                       ref Xorshift rnd)
in {
    assert(nPoints > 0);
    assert(radius > 0);
} out(result) {
    assert(result.length == nPoints);
    foreach (const ref p; result) {
        assert(p.cluster == 0);
        assert(!p.x.isNaN && !p.y.isNaN);
    }
} body {
    Point[] points;
    points.reserve(nPoints);

    // This is not a uniform 2D distribution.
    foreach (immutable i; 0 .. nPoints) {
        immutable r = uniform(0.0, radius, rnd);
        immutable ang = uniform(0.0, 2 * PI, rnd);
        points ~= Point(r * ang.cos, r * ang.sin); // Sincos?
    }

    return points;
}


struct ClusterCenter {
    double x, y;
    void opAssign(in ref Point p) pure nothrow {
        this.x = p.x;
        this.y = p.y;
    }
}


const(ClusterCenter)[] lloyd(Point[] points,
                             in size_t nclusters,
                             ref Xorshift rnd)
in {
    assert(points.length >= nclusters);
    assert(nclusters > 0);
    foreach (const ref p; points)
        assert(!p.x.isNaN && !p.y.isNaN);
} out(result) {
    assert(result.length == nclusters);
    foreach (const ref cc; result)
        assert(!cc.x.isNaN && !cc.y.isNaN);
} body {
    /// Distance and index of the closest cluster center.
    static Tuple!(size_t, double)
    nearestClusterCenter(in ref Point point,
                         in ClusterCenter[] centers) pure nothrow
    in {
        assert(centers.length > 0);
    } out(result) {
        assert(result[0] < centers.length);
        immutable ClusterCenter c = centers[result[0]];
        immutable d = (c.x - point.x) ^^ 2  +  (c.y - point.y) ^^ 2;
        assert(feqrel(cast()result[1], cast()d) > 45); // Arbitrary.
    } body {
        static double sqrDistance2D(in ref ClusterCenter a,
                                    in ref Point b) pure nothrow {
            return (a.x - b.x) ^^ 2 + (a.y - b.y) ^^ 2;
        }

        size_t minIndex = point.cluster;
        double minDist = double.max;

        foreach (immutable i, const ref cc; centers) {
            immutable d = sqrDistance2D(cc, point);
            if (minDist > d) {
                minDist = d;
                minIndex = i;
            }
        }

        return tuple(minIndex, minDist);
    }


    static void kMeansPP(Point[] points,
                         ClusterCenter[] centers,
                         ref Xorshift rnd)
    in {
        assert(points.length >= centers.length);
        assert(centers.length > 0);
    } body {
        centers[0] = points[uniform(0, $, rnd)];
        auto d = new double[points.length];

        foreach (immutable i; 1 .. centers.length) {
            double sum = 0;
            foreach (immutable j, const ref p; points) {
                d[j] = nearestClusterCenter(p, centers[0 .. i])[1];
                sum += d[j];
            }

            sum = uniform(0.0, sum, rnd);

            foreach (immutable j, immutable dj; d) {
                sum -= dj;
                if (sum > 0)
                    continue;
                centers[i] = points[j];
                break;
            }
        }

        foreach (ref p; points)
            // Implicit cast of Hconst!ClusterCenter
            // to ClusterCenter[].
            p.cluster = nearestClusterCenter(p, centers)[0];
    }


    auto centers = new ClusterCenter[nclusters];
    kMeansPP(points, centers, rnd);
    auto clusterSizes = new size_t[centers.length];

    size_t changed;
    do {
        // Find clusters centroids.
        centers[] = ClusterCenter(0, 0);
        clusterSizes[] = 0;

        foreach (immutable i, const ref p; points)
            with (centers[p.cluster]) {
                clusterSizes[p.cluster]++;
                x += p.x;
                y += p.y;
            }

        foreach (immutable i, ref cc; centers) {
            cc.x /= clusterSizes[i];
            cc.y /= clusterSizes[i];
        }

        // Find closest centroid of each point.
        changed = 0;
        foreach (ref p; points) {
            immutable minI = nearestClusterCenter(p, centers)[0];
            if (minI != p.cluster) {
                changed++;
                p.cluster = minI;
            }
        }
    // Stop when 99.9% of points are good.
    } while (changed > (points.length >> 10));

    return centers;
}


void printEps(in Point[] points, in ClusterCenter[] centers,
              in size_t W = 400, in size_t H = 400) nothrow
in {
    assert(points.length >= centers.length);
    assert(centers.length > 0);
    assert(W > 0 && H > 0);
    foreach (const ref p; points)
        assert(!p.x.isNaN && !p.y.isNaN);
    foreach (const ref cc; centers)
        assert(!cc.x.isNaN && !cc.y.isNaN);
} body {
    auto findBoundingBox() nothrow {
        double min_x, max_x, min_y, max_y;
        max_x = max_y = -double.max;
        min_x = min_y = double.max;

        foreach (const ref p; points) {
            if (max_x < p.x) max_x = p.x;
            if (min_x > p.x) min_x = p.x;
            if (max_y < p.y) max_y = p.y;
            if (min_y > p.y) min_y = p.y;
        }
        assert(max_x > min_x && max_y > min_y);

        return tuple(min(W / (max_x - min_x), H / (max_y - min_y)),
                     (max_x + min_x) / 2, (max_y + min_y) / 2);
    }
    //immutable (scale, cx, cy) = findBoundingBox();
    immutable sc_cx_cy = findBoundingBox();
    immutable double scale = sc_cx_cy[0];
    immutable double cx = sc_cx_cy[1];
    immutable double cy = sc_cx_cy[2];

    static immutable struct Color { immutable double r, g, b; }

    immutable size_t k = centers.length;
    Color[] colors;
    colors.reserve(centers.length);
    foreach (immutable i; 0 .. centers.length)
        colors ~= Color((3 * (i + 1) % k) / double(k),
                        (7 * i % k) / double(k),
                        (9 * i % k) / double(k));

    printf("%%!PS-Adobe-3.0\n%%%%BoundingBox: -5 -5 %d %d\n",
           W + 10, H + 10);

    printf("/l {rlineto} def /m {rmoveto} def\n" ~
           "/c { .25 sub exch .25 sub exch .5 0 360 arc fill } def\n" ~
           "/s { moveto -2 0 m 2 2 l 2 -2 l -2 -2 l closepath " ~
           "   gsave 1 setgray fill grestore gsave 3 setlinewidth" ~
           " 1 setgray stroke grestore 0 setgray stroke }def\n");

    foreach (immutable i, const ref cc; centers) {
        printf("%g %g %g setrgbcolor\n", colors[i].tupleof);

        foreach (const ref p; points) {
            if (p.cluster != i)
                continue;
            printf("%.3f %.3f c\n",
                   (p.x - cx) * scale + W / 2,
                   (p.y - cy) * scale + H / 2);
        }

        printf("\n0 setgray %g %g s\n",
               (cc.x - cx) * scale + W / 2,
               (cc.y - cy) * scale + H / 2);
    }

    "\n%%%%EOF".printf;
}


void main() {
    enum size_t nPoints = 100_000;
    enum size_t nClusters = 11; // k.
    auto rnd = 1.Xorshift; // For speed and repeatability.

    auto points = generatePoints(nPoints, 10, rnd);
    const clusterCenters = lloyd(points, nClusters, rnd);
    printEps(points, clusterCenters);
}
