class LoopMultiple
{
  public static Void main ()
  {
    List arr1 := ["a", "b", "c"]
    List arr2 := ["A", "B", "C"]
    List arr3 := [1, 2, 3]
    [arr1.size, arr2.size, arr3.size].min.times |Int i|
    {
      echo ("${arr1[i]}${arr2[i]}${arr3[i]}")
    }
  }
}
