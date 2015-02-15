class Main
{
  static Float arithmeticMean (Int[] nums)
  {
    if (nums.size == 0) return 0.0f
    sum := 0
    nums.each |n| { sum += n }
    return sum.toFloat / nums.size
  }

  static Float geometricMean (Int[] nums)
  {
    if (nums.size == 0) return 0.0f
    product := 1
    nums.each |n| { product *= n }
    return product.toFloat.pow(1f/nums.size)
  }

  static Float harmonicMean (Int[] nums)
  {
    if (nums.size == 0) return 0.0f
    reciprocals := 0f
    nums.each |n| { reciprocals += 1f / n }
    return nums.size.toFloat / reciprocals
  }

  public static Void main ()
  {
    items := (1..10).toList
    // display results
    echo (arithmeticMean (items))
    echo (geometricMean (items))
    echo (harmonicMean (items))
    // check given relation
    if ((arithmeticMean (items) >= geometricMean (items)) &&
        (geometricMean (items) >= harmonicMean (items)))
      echo ("relation holds")
    else
      echo ("relation failed")
  }
}
