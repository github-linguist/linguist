class FRange
{
  const Float low
  const Float high
  // in constructing a range, ensure the low value is smaller than high
  new make (Float low, Float high)
  {
    this.low = ( low <= high ? low : high )
    this.high = ( low <= high ? high : low )
  }

  // return range as a string
  override Str toStr () { "[$low,$high]" }

  // return a point in given range interpolated into this range
  Float remap (Float point, FRange given)
  {
    this.low + (point - given.low) * (this.high - this.low) / (given.high - given.low)
  }
}

class Main
{
  public static Void main ()
  {
    range1 := FRange (0f, 10f)
    range2 := FRange (-1f, 0f)
    11.times |Int n|
    {
      m := range2.remap (n.toFloat, range1)
      echo ("Value $n in ${range1} maps to $m in ${range2}")
    }
  }
}
