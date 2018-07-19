class Main
{
  static Void knuthShuffle (List array)
  {
    ((array.size-1)..1).each |Int i|
    {
      r := Int.random(0..i)
      array.swap (i, r)
    }
  }

  public static Void main ()
  {
    List a := [1,2,3,4,5]
    knuthShuffle (a)
    echo (a)

    List b := ["apples", "oranges", "pears", "bananas"]
    knuthShuffle (b)
    echo (b)
  }
}
