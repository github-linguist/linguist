import grayscale_image, bitmap_bresenhams_line_algorithm;

struct Pt { int x, y; } // Signed.

void quadraticBezier(size_t nSegments=20, Color)
                    (Image!Color im, in Pt p1, in Pt p2, in Pt p3,
                     in Color color)
pure nothrow if (nSegments > 0) {
    Pt[nSegments + 1] points = void;

    foreach (immutable i, ref p; points) {
        immutable double t = i / double(nSegments),
                         a = (1.0 - t) ^^ 2,
                         b = 2.0 * t * (1.0 - t),
                         c = t ^^ 2;
        p = Pt(cast(typeof(Pt.x))(a * p1.x + b * p2.x + c * p3.x),
               cast(typeof(Pt.y))(a * p1.y + b * p2.y + c * p3.y));
    }

    foreach (immutable i, immutable p; points[0 .. $ - 1])
        im.drawLine(p.x, p.y, points[i + 1].x, points[i + 1].y, color);
}

void main() {
    auto im = new Image!Gray(20, 20);
    im.clear(Gray.white);
    im.quadraticBezier(Pt(1,10), Pt(25,27), Pt(15,2), Gray.black);
    im.textualShow();
}
