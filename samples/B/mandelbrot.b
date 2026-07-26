/* https://github.com/bext-lang/b/blob/main/examples/mandelbrot.b */
W 80;
H 40;
IT 100;

f 1000;

gradient "@%#*+=-:. ";

m(cr,ci) {
    auto a, b, i, tmp;
    cr -= 3*W/4;
    ci -= H/2;

    cr = (cr * f * 3) / W;
    ci = (ci * f * 3) / H;

    a = b = 0;

    i = 0; while (i < IT) {
        if (a*a/f + b*b/f > 2*2*f) {
            return (9 - (i*10 / IT));
        }

        tmp = (a*a - b*b)/f + cr;
        b = (a*b)/f*2 + ci;
        a = tmp;

        i++;
    }

    return (0);
}

main() {
    extrn char, putchar;

    // we can't use more precision on 16-bit systems.
    if (&0[1] <= 2) {
        f = 50;
        IT = 90;
    }

    auto i, j;
    j = 0; while (j < H) {
        i = 0; while (i < W) {
            putchar(char(gradient, m(i,j)));
            i++;
        }
        putchar('\n');

        j++;
    }
}
