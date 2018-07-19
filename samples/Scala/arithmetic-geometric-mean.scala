  def agm(a: Double, g: Double, eps: Double): Double = {
    if (math.abs(a - g) < eps) (a + g) / 2
    else agm((a + g) / 2, math.sqrt(a * g), eps)
  }

  agm(1, math.sqrt(2)/2, 1e-15)
