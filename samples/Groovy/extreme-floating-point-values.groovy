def negInf = -1.0d / 0.0d;   //also Double.NEGATIVE_INFINITY
def inf = 1.0d / 0.0d;       //also Double.POSITIVE_INFINITY
def nan = 0.0d / 0.0d;       //also Double.NaN
def negZero = -2.0d / inf;

println("  Negative inf: " + negInf);
println("  Positive inf: " + inf);
println("           NaN: " + nan);
println("    Negative 0: " + negZero);
println("    inf + -inf: " + (inf + negInf));
println("       0 * NaN: " + (0 * nan));
println("    NaN == NaN: " + (nan == nan));
println("NaN equals NaN: " + (nan.equals(nan)));
