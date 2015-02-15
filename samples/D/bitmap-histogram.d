import grayscale_image;

Color findSingleChannelMedian(Color)(in Image!Color img)
nothrow if (Color.tupleof.length == 1) // Hack.
in {
    assert(img !is null);
} body {
    size_t[Color.max + 1] hist;
    foreach (immutable c; img.image)
        hist[c]++;

    // Slower indexes, but not significantly so.
    auto from = Color(0);
    auto to = Color(hist.length - 1);

    auto left = hist[from];
    auto right = hist[to];

    while (from != to)
        if (left < right) {
            from++;
            left += hist[from];
        } else {
            to--;
            right += hist[to];
        }

    return from;
}

Image!Color binarizeInPlace(Color)(Image!Color img,
                                   in Color thresh)
nothrow in {
    assert(img !is null);
} body {
    foreach (immutable i, ref c; img.image)
        c = (c < thresh) ? Color.min : Color.max;
    return img;
}

void main() {
    Image!RGB im;
    im.loadPPM6("quantum_frog.ppm");
    auto img = im.rgb2grayImage();
    img.binarizeInPlace(img.findSingleChannelMedian())
       .savePGM("quantum_frog_bin.pgm");
}
