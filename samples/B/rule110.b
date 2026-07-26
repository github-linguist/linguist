/* https://github.com/bext-lang/b/blob/main/examples/rule110.b */
// -*- mode: simpc -*-

word;

display(base, n) {
    extrn printf;
    auto i;

    i  = 0;
    while (i < n) {
        if (base[i]) printf("#"); else printf(".");
        i  += 1;
    }
    printf("\n");
}

next(base, n) {
    auto i, state;

    state = base[0] | base[1] << 1;
    i  = 2;
    while (i < n) {
        state <<= 1;
        state  |= base[i];
        state  &= 7;
        base[i - 1] = (110>>state)&1;
        i += 1;
    }
}

main() {
    extrn malloc, memset;
    auto base, n;

    word = &0[1]; /* trick to obtain the word size */
    n    = 100;
    base = malloc(word*n);
    memset(base, 0, word*n);
    base[n - 2] = 1;

    display(base, n);
    auto i;
    i = 0;
    while (i < n - 3) {
        next(base, n);
        display(base, n);
        i += 1;
    }
}
