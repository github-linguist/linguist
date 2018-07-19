class Main
{
  Void main ()
  {
    items := [1, 2, 3, 4, 5, 6, 7, 8]
    // create a new list with just the even numbers
    evens := items.findAll |i| { i.isEven }
    // display the result
    echo (evens.join(","))
  }
}
