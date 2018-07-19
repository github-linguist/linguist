def sum_of_squares(xs: Seq[Double]) = xs.foldLeft(0) {(a,x) => a + x*x}
