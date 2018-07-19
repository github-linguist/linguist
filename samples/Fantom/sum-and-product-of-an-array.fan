class Main
{
  public static Void main ()
  {
    Int[] array := (1..20).toList

    // you can use a loop
    Int sum := 0
    array.each |Int n| { sum += n }
    echo ("Sum of array is : $sum")

    Int product := 1
    array.each |Int n| { product *= n }
    echo ("Product of array is : $product")

    // or use 'reduce'
    // 'reduce' takes a function,
    //       the first argument is the accumulated value
    //       and the second is the next item in the list
    sum = array.reduce(0) |Obj r, Int v -> Obj|
    {
      return (Int)r + v
    }
    echo ("Sum of array : $sum")

    product = array.reduce(1) |Obj r, Int v -> Obj|
    {
      return (Int)r * v
    }
    echo ("Product of array : $product")
  }
}
