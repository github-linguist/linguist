class MontyCarlo
{
  // assume square/circle of width 1 unit
  static Float findPi (Int samples)
  {
    Int insideCircle := 0
    samples.times
    {
      x := Float.random
      y := Float.random
      if ((x*x + y*y).sqrt <= 1.0f) insideCircle += 1
    }
    return insideCircle * 4.0f / samples
  }

  public static Void main ()
  {
    [100, 1000, 10000, 1000000, 10000000].each |sample|
    {
      echo ("Sample size $sample gives PI as ${findPi(sample)}")
    }
  }
}
