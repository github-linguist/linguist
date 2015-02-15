import grayscale_image, bitmap_bresenhams_line_algorithm;

struct Pt { int x, y; } // Signed.

void cubicBezier(size_t nSegments=20, Color)
                (Image!Color im,
                 in Pt p1, in Pt p2, in Pt p3, in Pt p4,
                 in Color color)
pure nothrow if (nSegments > 0) {
    Pt[nSegments + 1] points = void;

    foreach (immutable i, ref p; points) {
        immutable double t = i / double(nSegments),
                         a = (1.0 - t) ^^ 3,
                         b = 3.0 * t * (1.0 - t) ^^ 2,
                         c = 3.0 * t ^^ 2 * (1.0 - t),
                         d = t ^^ 3;

        alias T = typeof(Pt.x);
        p = Pt(cast(T)(a * p1.x + b * p2.x + c * p3.x + d * p4.x),
               cast(T)(a * p1.y + b * p2.y + c * p3.y + d * p4.y));
    }

    foreach (immutable i, immutable p; points[0 .. $ - 1])
        im.drawLine(p.x, p.y, points[i + 1].x, points[i + 1].y, color);
}

void main() {
    auto im = new Image!Gray(17, 17);
    im.clear(Gray.white);
    im.cubicBezier(Pt(16, 1), Pt(1, 4), Pt(3, 16), Pt(15, 11),
                   Gray.black);
    im.textualShow();
}
