import bitmap: Image, RGB;

void circle(Color)(Image!Color img, in int x0, in int y0,
                   in int radius, in Color color) pure nothrow {
    int f = 1 - radius;
    int ddfX = 1;
    int ddfY = -2 * radius;
    int x = 0;
    int y = radius;
    img[x0, y0 + radius] = color;
    img[x0, y0 - radius] = color;
    img[x0 + radius, y0] = color;
    img[x0 - radius, y0] = color;

    while (x < y) {
        if (f >= 0) {
            y--;
            ddfY += 2;
            f += ddfY;
        }
        x++;
        ddfX += 2;
        f += ddfX;
        img[x0 + x, y0 + y] = color;
        img[x0 - x, y0 + y] = color;
        img[x0 + x, y0 - y] = color;
        img[x0 - x, y0 - y] = color;
        img[x0 + y, y0 + x] = color;
        img[x0 - y, y0 + x] = color;
        img[x0 + y, y0 - x] = color;
        img[x0 - y, y0 - x] = color;
    }
}

void main() {
    auto img = new Image!RGB(25, 25);
    img.clear(RGB.white);
    circle(img, 12, 12, 12, RGB.black);
    img.textualShow();
}
