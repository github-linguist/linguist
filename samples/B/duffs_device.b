/* https://github.com/bext-lang/b/blob/main/examples/duffs_device.b */
/* // https://en.wikipedia.org/wiki/Duff%27s_device */
duffs_device(s) {
    extrn strlen, putchar;
    auto n, count;
    count = strlen(s);
    n = (count + 7) / 8;
    switch count%8 {
    case 0: while(1) { putchar(*s++);
    case 7:            putchar(*s++);
    case 6:            putchar(*s++);
    case 5:            putchar(*s++);
    case 4:            putchar(*s++);
    case 3:            putchar(*s++);
    case 2:            putchar(*s++);
    case 1:            putchar(*s++);
                       putchar('|');
                       putchar(10);
                       if (--n <= 0) return(0);
            }
    }
}

main() {
    duffs_device("The quick brown fox jumps over the lazy dog.");
}
