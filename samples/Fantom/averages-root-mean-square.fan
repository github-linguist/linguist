class Main
{
  static Float averageRms (Float[] nums)
  {
    if (nums.size == 0) return 0.0f
    Float sum := 0f
    nums.each { sum += it * it }
    return (sum / nums.size.toFloat).sqrt
  }

  public static Void main ()
  {
    a := [1f,2f,3f,4f,5f,6f,7f,8f,9f,10f]
    echo ("RMS Average of $a is: " + averageRms(a))
  }
}
