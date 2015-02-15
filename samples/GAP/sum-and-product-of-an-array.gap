v := [1 .. 8];

Sum(v);
# 36

Product(v);
# 40320

# You can sum or multiply the result of a function

Sum(v, n -> n^2);
# 204

Product(v, n -> 1/n);
# 1/40320
