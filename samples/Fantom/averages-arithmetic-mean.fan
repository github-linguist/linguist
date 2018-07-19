class Main
{
  static Float average (Float[] nums)
  {
    if (nums.size == 0) return 0.0f
    Float sum := 0f
    nums.each |num| { sum += num }
    return sum / nums.size.toFloat
  }

  public static Void main ()
  {
    [[,], [1f], [1f,2f,3f,4f]].each |Float[] i|
    {
      echo ("Average of $i is: " + average(i))
    }
  }
}
