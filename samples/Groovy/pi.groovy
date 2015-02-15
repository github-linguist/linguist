BigInteger q = 1, r = 0, t = 1, k = 1, n = 3, l = 3
String nn
boolean first = true

while (true) {
    (nn, first, q, r, t, k, n, l) = (4*q + r - t < n*t) \
        ? ["${n}${first?'.':''}", false, 10*q, 10*(r - n*t), t  , k    , 10*(3*q + r)/t - 10*n    , l    ] \
        : [''                   , first, q*k , (2*q + r)*l , t*l, k + 1, (q*(7*k + 2) + r*l)/(t*l), l + 2]
    print nn
}
