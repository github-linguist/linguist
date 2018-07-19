module bitmap_bresenhams_line_algorithm;

import std.algorithm, std.math, bitmap;

void drawLine(Color)(Image!Color img,
                        size_t x1,    size_t y1,
                     in size_t x2, in size_t y2,
                     in Color color)
pure nothrow {
    immutable int dx = x2 - x1;
    immutable int ix = (dx > 0) - (dx < 0);
    immutable size_t dx2 = abs(dx) * 2;
    int dy = y2 - y1;
    immutable int iy = (dy > 0) - (dy < 0);
    immutable size_t dy2 = abs(dy) * 2;
    img[x1, y1] = color;

    if (dx2 >= dy2) {
        int error = dy2 - (dx2 / 2);
        while (x1 != x2) {
            if (error >= 0 && (error || (ix > 0))) {
                error -= dx2;
                y1 += iy;
            }

            error += dy2;
            x1 += ix;
            img[x1, y1] = color;
        }
    } else {
        int error = dx2 - (dy2 / 2);
        while (y1 != y2) {
            if (error >= 0 && (error || (iy > 0))) {
                error -= dy2;
                x1 += ix;
            }

            error += dx2;
            y1 += iy;
            img[x1, y1] = color;
        }
    }
}

version (bitmap_bresenhams_line_algorithm_main) {
    void main() {
        auto img = new Image!RGB(25, 22);
        img.drawLine(5, 5, 15, 20, RGB.white);
        img.drawLine(3, 20, 10, 12, RGB.white);
        img.textualShow();
    }
}
