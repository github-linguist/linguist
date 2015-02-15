:- use_module(library(pce)).

mandelbrot :-
    new(D, window('Mandelbrot Set')),
    send(D, size, size(700, 650)),
    new(Img, image(@nil, width := 700, height := 650, kind := pixmap)),

    forall(between(0,699, I),
           (   forall(between(0,649, J),
              (   get_RGB(I, J, R, G, B),
                  R1 is (R * 256) mod 65536,
                  G1 is (G * 256) mod 65536,
                  B1 is (B * 256) mod 65536,
                  send(Img, pixel(I, J, colour(@default, R1, G1, B1))))))),
    new(Bmp, bitmap(Img)),
    send(D, display, Bmp, point(0,0)),
    send(D, open).

get_RGB(X, Y, R, G, B) :-
    CX is (X - 350) / 150,
    CY is (Y - 325) / 150,
    Iter = 570,
    compute_RGB(CX, CY, 0, 0, Iter, It),
    IterF is It \/ It << 15,
    R is IterF >> 16,
    Iter1 is IterF - R << 16,
    G is Iter1 >> 8,
    B  is Iter1 - G << 8.

compute_RGB(CX, CY, ZX, ZY, Iter, IterF) :-
    ZX * ZX + ZY * ZY < 4,
    Iter > 0,
    !,
    Tmp is  ZX * ZX - ZY * ZY + CX,
    ZY1 is 2 * ZX * ZY + CY,
    Iter1 is Iter - 1,
    compute_RGB(CX, CY, Tmp, ZY1, Iter1, IterF).

compute_RGB(_CX, _CY, _ZX, _ZY, Iter, Iter).
