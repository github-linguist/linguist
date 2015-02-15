import grayscale_image;

void main() {
    enum order = 8,
         margin = 10,
         width = 2 ^^ order;

    auto im = new Image!Gray(width + 2 * margin, width + 2 * margin);
    im.clear(Gray.white);

    foreach (y; 0 .. width)
        foreach (x; 0 .. width)
            if ((x & y) == 0)
                im[x + margin, y + margin] = Gray.black;
    im.savePGM("sierpinski.pgm");
}
