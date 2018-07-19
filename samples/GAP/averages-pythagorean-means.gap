# The first two work with rationals or with floats
# (but bear in mind that support of floating point is very poor in GAP)
mean := v -> Sum(v) / Length(v);
harmean := v -> Length(v) / Sum(v, Inverse);
geomean := v -> EXP_FLOAT(Sum(v, LOG_FLOAT) / Length(v));

mean([1 .. 10]);
# 11/2
harmean([1 .. 10]);
# 25200/7381

v := List([1..10], FLOAT_INT);;
mean(v);
# 5.5
harmean(v);
# 3.41417
geomean(v);
# 4.52873
