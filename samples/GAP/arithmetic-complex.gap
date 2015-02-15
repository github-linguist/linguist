# GAP knows gaussian integers, gaussian rationals (i.e. Q[i]), and cyclotomic fields. Here are some examples.
# E(n) is an nth primitive root of 1
i := Sqrt(-1);
# E(4)
(3 + 2*i)*(5 - 7*i);
# 29-11*E(4)
1/i;
# -E(4)
Sqrt(-3);
# E(3)-E(3)^2

i in GaussianIntegers;
# true
i/2 in GaussianIntegers;
# false
i/2 in GaussianRationals;
# true
Sqrt(-3) in Cyclotomics;
# true
